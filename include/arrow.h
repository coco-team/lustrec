
#ifndef _ARROW
#define _ARROW

struct _arrow_mem {struct _arrow_reg {_Bool _first; } _reg; };

extern struct _arrow_mem *arrow_alloc ();

#define _arrow_DECLARE(inst)\
  struct _arrow_mem inst;
  
#define _arrow_LINK(inst) do {\
  ;\
} while (0)

#define _arrow_step(x,y,output,self) ((self)->_reg._first?((self)->_reg._first=0,(*output = x)):(*output = y))

#define _arrow_reset(self) {(self)->_reg._first = 1;}

#endif
