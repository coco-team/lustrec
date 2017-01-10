#ifndef _IO_FRONTEND
#define _IO_FRONTEND

/* Print a promt ? ************************/
extern int ISATTY;

/* Standard Input procedures **************/

/*@ assigns *n; */
extern _Bool _get_bool(FILE* file, char* n);

/*@ assigns *n; */
extern int _get_int(FILE* file, char* n);

/*@ assigns *n; */
extern double _get_double(FILE* file, char* n);

/* Standard Output procedures **************/
/*@ assigns \nothing; */
extern void _put_bool(FILE* file, char* n, _Bool _V);

/*@ assigns \nothing; */
extern void _put_int(FILE* file, char* n, int _V);

/*@ assigns \nothing; */
extern void _put_double(FILE* file, char* n, double _V);

#endif
