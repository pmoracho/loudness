#include <Rcpp.h>

#ifdef WIN32         // means WIN64, too
#undef Realloc
#undef Free
#include <windows.h>
#include <stdio.h>
#include <tchar.h>
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>   // for open()
#include <unistd.h>  // for close()
#endif

using namespace Rcpp;

inline int nrow(const char* x, int sz) {
  const char* ptr = &x[0];
  int nrow = 0;
  while (ptr < x + sz) {
    nrow += *ptr == '\n';
    ++ptr;
  }
  return nrow;
}

//' read
//'
//' Generic fast read
//'
//' @param path File path
//' @param lines As text lines file
//' @importFrom Rcpp evalCpp
//' @export
// [[Rcpp::export]]
SEXP read(std::string path, bool lines) {

  using namespace std;

  const char eol = '\n';
  char* map;

  #ifndef WIN32
  struct stat file_info;

  int fd = open( path.c_str(), O_RDONLY );
  if (fstat(fd, &file_info) == -1) {
    stop("Could not read file information.");
  }
  int sz = file_info.st_size;
  if (sz <= 0) {
    SEXP output = Rf_allocVector(STRSXP, 0);
    return output;
  }
  #ifdef MAP_POPULATE
  map = (char*) mmap(0, sz, PROT_READ, MAP_SHARED | MAP_POPULATE, fd, 0);
  #else
  map = (char*) mmap(0, sz, PROT_READ, MAP_SHARED, fd, 0);
  #endif

  if (map == MAP_FAILED) {
    close(fd);
    stop("Error mapping the file.");
  }

  #else
  // tactlessly borrowed from data.table
  // Following: http://msdn.microsoft.com/en-gb/library/windows/desktop/aa366548(v=vs.85).aspx
  HANDLE hFile=0;
  HANDLE hMap=0;
  DWORD dwFileSize=0;
  const char* fnam = path.c_str();
  hFile = CreateFile(fnam, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
  if (hFile==INVALID_HANDLE_VALUE) Rf_error("File not found: %s",fnam);
  dwFileSize=GetFileSize(hFile,NULL);
  if (dwFileSize==0) { CloseHandle(hFile); Rf_error("File is empty: %s", fnam); }
  size_t filesize = (size_t)dwFileSize;
  int sz = (int) filesize;
  hMap=CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL); // dwFileSize+1 not allowed here, unlike mmap where +1 is zero'd
  if (hMap==NULL) { CloseHandle(hFile); Rf_error("This is Windows, CreateFileMapping returned error %d for file %s", GetLastError(), fnam); }
  map = (char *)MapViewOfFile(hMap,FILE_MAP_READ,0,0,dwFileSize);
  if (map == NULL) {
      CloseHandle(hMap);
      CloseHandle(hFile);
  }
  #endif

	SEXP output;

  // split by '\n'?
  if (lines) {

    // incomplete final line?
    bool read_last_line = false;
    char last_char = *(map + sz - 1);
    if (last_char != '\n' && last_char != '\r') {
      Rf_warning("incomplete final line found on '%s'", path.c_str());
      read_last_line = true;
    }

    if (strchr(map, '\n') == NULL && strchr(map, '\r') == NULL) {
      Rf_warning("incomplete final line found on '%s'", path.c_str());
      PROTECT( output = Rf_allocVector(STRSXP, 1) );
      SET_STRING_ELT(output, 0, Rf_mkCharLen(map, sz));
    } else {
      int n = nrow(map, sz);
      if (read_last_line) {
        ++n;
      }
      output = PROTECT( Rf_allocVector(STRSXP, n) );
      char* pch_old = &map[0];
      char* pch;
      pch = strchr(map, eol);
      // avoid reading \r
      int pch_incr = 1;
      if (*(pch-1) == '\r') {
        --pch;
        ++pch_incr;
      }
      int i = 0;
      while (pch != NULL) {
        if (*(pch-1) == '\r') {
          --pch;
        }
        SET_STRING_ELT(output, i, Rf_mkCharLen(pch_old, (int)((size_t) pch - (size_t) pch_old)));
        pch_old = pch + pch_incr;
        pch = strchr(pch_old, eol);
        ++i;
      }
      if (read_last_line) {
        SET_STRING_ELT(output, n-1, Rf_mkChar(pch_old));
      }
    }
  } else {
    output = PROTECT( Rf_allocVector(STRSXP, 1) );
    SET_STRING_ELT(output, 0, Rf_mkCharLen(map, sz));
  }

#ifndef WIN32
  munmap(map, sz);
	close(fd);
#else
  UnmapViewOfFile(map);
  CloseHandle(hMap);
  CloseHandle(hFile);
#endif

  UNPROTECT(1);
  return output;

}

//' Multiply a number by two
//'
//' @param x A single integer.
//' @export
// [[Rcpp::export]]
int timesTwo(int x) {
  return x * 2;
}
