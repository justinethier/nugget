

static char *transport(x) char *x;
/* Transport one object.  WARNING: x cannot be nil!!! */
{switch (type_of(x))
   {case cons_tag:
      {register list nx = (list) allocp;
       type_of(nx) = cons_tag; car(nx) = car(x); cdr(nx) = cdr(x);
       forward(x) = nx; type_of(x) = forward_tag;
       allocp = ((char *) nx)+sizeof(cons_type);
       return (char *) nx;}
    case closure0_tag:
      {register closure0 nx = (closure0) allocp;
       type_of(nx) = closure0_tag; nx->fn = ((closure0) x)->fn;
       forward(x) = nx; type_of(x) = forward_tag;
       allocp = ((char *) nx)+sizeof(closure0_type);
       return (char *) nx;}
    case closure1_tag:
      {register closure1 nx = (closure1) allocp;
       type_of(nx) = closure1_tag; nx->fn = ((closure1) x)->fn;
       nx->elt1 = ((closure1) x)->elt1;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure1_type);
       return (char *) nx;}
    case closure2_tag:
      {register closure2 nx = (closure2) allocp;
       type_of(nx) = closure2_tag; nx->fn = ((closure2) x)->fn;
       nx->elt1 = ((closure2) x)->elt1;
       nx->elt2 = ((closure2) x)->elt2;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure2_type);
       return (char *) nx;}
    case closure3_tag:
      {register closure3 nx = (closure3) allocp;
       type_of(nx) = closure3_tag; nx->fn = ((closure3) x)->fn;
       nx->elt1 = ((closure3) x)->elt1;
       nx->elt2 = ((closure3) x)->elt2;
       nx->elt3 = ((closure3) x)->elt3;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure3_type);
       return (char *) nx;}
    case closure4_tag:
      {register closure4 nx = (closure4) allocp;
       type_of(nx) = closure4_tag; nx->fn = ((closure4) x)->fn;
       nx->elt1 = ((closure4) x)->elt1;
       nx->elt2 = ((closure4) x)->elt2;
       nx->elt3 = ((closure4) x)->elt3;
       nx->elt4 = ((closure4) x)->elt4;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure4_type);
       return (char *) nx;}
    case forward_tag:
       return (char *) forward(x);
    case symbol_tag:
    default:
      printf("transport: bad tag x=%p x.tag=%ld\n",(void *)x,type_of(x)); exit(0);}
 return x;}

/* Use overflow macro which already knows which way the stack goes. */
/* Major collection, transport objects on stack or old heap */
#define transp(p) \
temp = (p); \
if ((check_overflow(low_limit,temp) && \
     check_overflow(temp,high_limit)) || \
    (check_overflow(old_heap_low_limit, temp) && \
     check_overflow(temp,old_heap_high_limit))) \
   (p) = (object) transport(temp);

static void GC_loop(int major, closure cont, object *ans, int num_ans)
{char foo;
 int i;
 register object temp;
 register object low_limit = &foo; /* Move live data above us. */
 register object high_limit = stack_begin;
 register char *scanp = allocp; /* Cheney scan pointer. */
 register object old_heap_low_limit = low_limit; // Minor-GC default
 register object old_heap_high_limit = high_limit; // Minor-GC default

 char *tmp_bottom = bottom;    /* Bottom of tospace. */
 char *tmp_allocp = allocp;    /* Cheney allocate pointer. */
 char *tmp_alloc_end = alloc_end;
 if (major) {
    // Initialize new heap (TODO: make a function for this)
    bottom = calloc(1,global_heap_size);
    allocp = (char *) ((((long) bottom)+7) & -8);
    alloc_end = allocp + global_heap_size - 8;
    scanp = allocp;
    old_heap_low_limit = tmp_bottom;
    old_heap_high_limit = tmp_alloc_end;
 }

 /* Transport GC's continuation and its argument. */
 transp(cont);
 gc_cont = cont;
 gc_num_ans = num_ans;
 for (i = 0; i < num_ans; i++){ 
     transp(ans[i]);
     gc_ans[i] = ans[i];
 }
 /* Transport global variable. */
 transp(unify_subst);
 while (scanp<allocp)       /* Scan the newspace. */
   switch (type_of(scanp))
     {case cons_tag:
        transp(car(scanp)); transp(cdr(scanp));
        scanp += sizeof(cons_type); break;
      case closure0_tag:
        scanp += sizeof(closure0_type); break;
      case closure1_tag:
        transp(((closure1) scanp)->elt1);
        scanp += sizeof(closure1_type); break;
      case closure2_tag:
        transp(((closure2) scanp)->elt1); transp(((closure2) scanp)->elt2);
        scanp += sizeof(closure2_type); break;
      case closure3_tag:
        transp(((closure3) scanp)->elt1); transp(((closure3) scanp)->elt2);
        transp(((closure3) scanp)->elt3);
        scanp += sizeof(closure3_type); break;
      case closure4_tag:
        transp(((closure4) scanp)->elt1); transp(((closure4) scanp)->elt2);
        transp(((closure4) scanp)->elt3); transp(((closure4) scanp)->elt4);
        scanp += sizeof(closure4_type); break;
      case symbol_tag: default:
        printf("GC: bad tag scanp=%p scanp.tag=%ld\n",(void *)scanp,type_of(scanp));
        exit(0);}

 if (major) {
     free(tmp_bottom);
 }
}

static void GC(cont,ans,num_ans) closure cont; object *ans; int num_ans;
{
 // JAE - Only room for one more minor-GC, so do a major one.
 // Not sure this is the best strategy, it may be better to do major
 // ones sooner, perhaps after every x minor GC's.
 //
 // Also may need to consider dynamically increasing heap size, but
 // by how much (1.3x, 1.5x, etc) and when? I suppose when heap usage
 // after a collection is above a certain percentage, then it would be 
 // necessary to increase heap size the next time.
 //
 if (allocp >= (bottom + (global_heap_size - global_stack_size))) {
     //printf("Possibly only room for one more minor GC. no_gcs = %ld\n", no_gcs);
     GC_loop(1, cont, ans, num_ans);
 } else {
     no_gcs++; /* Count the number of minor GC's. */
     GC_loop(0, cont, ans, num_ans);
 }
 // /JAE
 longjmp(jmp_main,1); /* Return globals gc_cont, gc_ans. */
}

/* This heap cons is used only for initialization. */
static list mcons(a,d) object a,d;
{register cons_type *c = malloc(sizeof(cons_type));
 c->tag = cons_tag; c->cons_car = a; c->cons_cdr = d;
 return c;}

static void main_main (stack_size,heap_size,stack_base)
     long stack_size,heap_size; char *stack_base;
{char in_my_frame;
 mclosure0(clos_exit,&my_exit);  /* Create a closure for exit function. */
 gc_ans[0] = &clos_exit;            /* It becomes the argument to test. */
 gc_num_ans = 1;
 /* Allocate stack buffer. */
 stack_begin = stack_base;
#if STACK_GROWS_DOWNWARD
 stack_limit1 = stack_begin - stack_size;
 stack_limit2 = stack_limit1 - 2000;
#else
 stack_limit1 = stack_begin + stack_size;
 stack_limit2 = stack_limit1 + 2000;
#endif
 printf("main: sizeof(cons_type)=%ld\n",(long) sizeof(cons_type));
 if (check_overflow(stack_base,&in_my_frame))
   {printf("main: Recompile with STACK_GROWS_DOWNWARD set to %ld\n",
           (long) (1-STACK_GROWS_DOWNWARD)); exit(0);}
 printf("main: stack_size=%ld  stack_base=%p  stack_limit1=%p\n",
        stack_size,(void *)stack_base,(void *)stack_limit1);
 printf("main: Try different stack sizes from 4 K to 1 Meg.\n");
 /* Do initializations of Lisp objects and rewrite rules. */
 quote_list_f = mlist1(quote_f); quote_list_t = mlist1(quote_t);
 /* Make temporary short names for certain atoms. */
 {

  /* Define the rules, but only those that are actually referenced. */

  /* Create closure for the test function. */
  mclosure0(run_test,&test);
  gc_cont = &run_test;
  /* Initialize constant expressions for the test runs. */

  /* Allocate heap area for second generation. */
  /* Use calloc instead of malloc to assure pages are in main memory. */
  printf("main: Allocating and initializing heap...\n");
  bottom = calloc(1,heap_size);
  allocp = (char *) ((((long) bottom)+7) & -8);
  alloc_end = allocp + heap_size - 8;
  printf("main: heap_size=%ld  allocp=%p  alloc_end=%p\n",
         (long) heap_size,(void *)allocp,(void *)alloc_end);
  printf("main: Try a larger heap_size if program bombs.\n");
  printf("Starting...\n");
  start = clock(); /* Start the timing clock. */
  /* These two statements form the most obscure loop in the history of C! */
  setjmp(jmp_main);
  if (gc_num_ans == 1) {
      funcall1((closure) gc_cont,gc_ans[0]);
  } else if (gc_num_ans == 2) {
      funcall2((closure) gc_cont,gc_ans[0],gc_ans[1]);
  } else {
      printf("Unsupported number of args from GC %d\n", gc_num_ans);
  }
  /*                                                                      */
  printf("main: your setjmp and/or longjmp are broken.\n"); exit(0);}}

static long long_arg(argc,argv,name,dval)
     int argc; char **argv; char *name; long dval;
{int j;
 for(j=1;(j+1)<argc;j += 2)
   if (strcmp(name,argv[j]) == 0)
     return(atol(argv[j+1]));
 return(dval);}

main(int argc,char **argv)
{long stack_size = long_arg(argc,argv,"-s",STACK_SIZE);
 long heap_size = long_arg(argc,argv,"-h",HEAP_SIZE);
 global_stack_size = stack_size;
 global_heap_size = heap_size;
 main_main(stack_size,heap_size,(char *) &stack_size);
 return 0;}
