/*
 Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa
 Copyright (C) 2024 Camm Maguire

This file is part of GNU Common Lisp, herein referred to as GCL

GCL is free software; you can redistribute it and/or modify it under
the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
License for more details.

You should have received a copy of the GNU Library General Public License 
along with GCL; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/
#ifndef COM_LENG
#define COM_LENG
#endif

/*  alloc.c  */
object alloc_object();
void *malloc(size_t);
void *realloc(void *,size_t);
/* void * memalign(size_t,size_t); */
void *alloc_contblock(size_t);
void *alloc_relblock(size_t);
/* object fSallocate_contiguous_pages(); */
/* object fSallocate_relocatable_pages(); */

/*  array.c  */
/* enum aelttype Iarray_element_type(); */
object fLrow_major_aref();
object fSaset1();
EXTER object sLarray_dimension_limit;
EXTER object sLarray_total_size_limit;
object fSmake_array1();
/* object fSmake_vector1(); */

/*  assignment.c  */
/* object setf(); */



/*  backq.c  */
EXTER int backq_level;
EXTER object sLlistA;
EXTER object sLappend;
EXTER object sLnconc;


/*  bds.c  */

/*  big.c  */
object make_integer_clear();
object stretch_big();
object copy_big();
object copy_to_big();
object big_minus();
object big_plus();
object big_times();
object normalize_big_to_object();
double big_to_double();
EXTER  struct bignum big_fixnum1_body,big_fixnum2_body,big_fixnum3_body,big_fixnum4_body,big_fixnum5_body;
EXTER object big_fixnum1,big_fixnum2,big_fixnum3,big_fixnum4,big_fixnum5;
object maybe_replace_big();




/* bind.c */
EXTER object ANDoptional;
EXTER object ANDrest;
EXTER object ANDkey;
EXTER object ANDallow_other_keys;
EXTER object ANDaux;
EXTER object sKallow_other_keys;
object find_special();
object let_bind();
object letA_bind();

/* block.c */

/*  cfun.c  */
object make_cfun();
object MF();
/* object MM(); */
/* object make_function_internal(); */
/* object make_si_function_internal(); */
/* object make_special_form_internal(); */
object make_macro();
object make_cclosure_new();

/*  character.d  */
EXTER object STreturn;
EXTER object STspace;
EXTER object STrubout;
EXTER object STpage;
EXTER object STtab;
EXTER object STbackspace;
EXTER object STlinefeed;
EXTER object STnewline;
object coerce_to_character();

/*  catch.c  */

/*  cmpaux.c  */
char object_to_char();
char *object_to_string();
float object_to_float();
double object_to_double();

/*  error.c  */
EXTER object sKerror,sKparse_error,sKreader_error,sKprogram_error;
EXTER object sKwrong_type_argument;
EXTER object sKcontrol_error;
EXTER object sKcatch;
EXTER object sKprotect;
EXTER object sKcatchall;
EXTER object sKdatum;
EXTER object sKexpected_type;
EXTER object sKpackage;
EXTER object sKformat_control;
EXTER object sKformat_arguments;
object wrong_type_argument();
EXTER object sSuniversal_error_handler;
EXTER object sSPminus_most_negative_fixnumP;

/*  eval.c  */
EXTER object sLapply;
EXTER object sLfuncall;
object simple_lispcall();
object simple_lispcall_no_event();
object simple_symlispcall();
object simple_symlispcall_no_event();
EXTER object siVevalhook;
EXTER object siVapplyhook;
object ieval();
object ifuncall(object,int,...);
object ifuncall1();
object ifuncall2();
object ifuncall3();
/* object fcalln1(); */
/* #define fcalln ((object (*)())fcalln1) */
object Ieval();
object Imacro_expand1();

/*  unixfasl.c  fasload.c  */

/*  file.d  */
EXTER object sKabort;
EXTER object sKappend;
EXTER object sKcreate;
EXTER object sKdefault;
EXTER object sKdirection;
EXTER object sKelement_type;
EXTER object sKif_does_not_exist;
EXTER object sKif_exists;
EXTER object sKinput;
EXTER object sKio;
EXTER object sKnew_version;
EXTER object sKoutput;
EXTER object sKoverwrite;
EXTER object sKprint;
EXTER object sKprobe;
EXTER object sKrename;
EXTER object sKrename_and_delete;
EXTER object sKset_default_pathname;
EXTER object sKsupersede;
EXTER object sKverbose;

EXTER object sLAstandard_inputA;
EXTER object sLAstandard_outputA;
EXTER object sLAerror_outputA;
EXTER object sLAquery_ioA;
EXTER object sLAdebug_ioA;
EXTER object sLAterminal_ioA;
EXTER object sLAtrace_outputA;
EXTER object terminal_io;
EXTER object standard_io;
EXTER object standard_error;

EXTER object sLAload_verboseA;
EXTER object FASL_string;
/* object stream_element_type(); */
object open_stream();
/* object make_two_way_stream(); */
/* object make_echo_stream(); */
object make_string_input_stream();
object make_string_output_stream();
/* object get_output_stream_string(); */
object read_fasl_data();

#ifdef UNIX
/*  unixfsys.c  */
FILE *backup_fopen();
#else
/*  filesystem.c  */
FILE *backup_fopen();
#endif

/*  frame.c  */
frame_ptr frs_sch();
frame_ptr frs_sch_catch();

/*  gbc.c  */
EXTER bool GBC_enable;

#ifdef CAN_UNRANDOMIZE_SBRK
EXTER bool gcl_unrandomized;
#endif

/*  let.c  */

/*  lex.c  */
object assoc_eq();
object lex_tag_sch();
object lex_block_sch();

/*  list.d  */
EXTER object sKtest;
EXTER object sKtest_not;
EXTER object sKkey;
EXTER object sKinitial_element;
/* EXTER object sKrev; */
object car();
object cdr();
object kdr();
object caar();
object cadr();
object cdar();
object cddr();
object caaar();
object caadr();
object cadar();
object caddr();
object cdaar();
object cdadr();
object cddar();
object cdddr();
object caaaar();
object caaadr();
object caadar();
object caaddr();
object cadaar();
object cadadr();
object caddar();
object cadddr();
object cdaaar();
object cdaadr();
object cdadar();
object cdaddr();
object cddaar();
object cddadr();
object cdddar();
object cddddr();
object nth();
object nthcdr();
object make_cons1();
object list(fixnum,...);
object listA(fixnum,...);
object append();
object copy_list();
object make_list();
object nconc();
object sublis1();

/*  macros.c  */
EXTER object sLAmacroexpand_hookA;
EXTER object sSdefmacroA;
object macro_expand();

/*  main.c  */
EXTER char * system_directory;
EXTER int ARGC;
EXTER char **ARGV;
void error();
#ifdef UNIX
EXTER char **ENVP;
#endif
object vs_overflow(void);

EXTER object sSAsystem_directoryA;
#ifdef UNIX
EXTER char *kcl_self;
#endif
#if !defined(IN_MAIN) || !defined(ATT)
EXTER bool raw_image;
#endif
char *merge_system_directory();


EXTER object sLquote;

EXTER object sLlambda;

EXTER object sSlambda_block;
EXTER object sSlambda_closure;
EXTER object sSlambda_block_closure;

EXTER object sLfunction;
EXTER object sSmacro;
EXTER object sStag;
EXTER object sLblock;


/*  mapfun.c  */

/*  multival.c  */

/*  number.c  */
EXTER object shortfloat_zero;
EXTER object longfloat_zero;
/* #define make_fixnum(a) ({fixnum _a=(a);((_a+SMALL_FIXNUM_LIMIT)&(-2*SMALL_FIXNUM_LIMIT))==0?small_fixnum(_a):make_fixnum1(_a);}) */
object make_fixnum1(long);
object make_ratio();
object make_shortfloat(float);
object make_longfloat();
object make_complex();
double number_to_double();
long fixint(object);
/*  num_pred.c  */

/*  num_comp.c  */

/*  num_arith  */
object bignum2();
object bignum3();
/* object number_to_complex(); */
object complex_plus();
object number_plus();
object number_negate();
object number_minus();
object number_times();
object number_divide();
object number_expt();
object integer_divide1();
object get_gcd();
object get_lcm();
object one_plus();
object one_minus();
object fixnum_add();
object fixnum_sub();
object new_bignum();

/*  num_co.c  */
object double_to_integer();

/*  num_log.c  */
object shift_integer();

/*  package.d  */
EXTER object lisp_package;
EXTER object user_package;
EXTER object keyword_package;
EXTER object system_package;
EXTER object gmp_package;
EXTER object sLApackageA;
EXTER object sKinternal;
EXTER object sKexternal;
EXTER object sKinherited;
EXTER object sKnicknames;
EXTER object sKuse;
EXTER int intern_flag;
EXTER object uninterned_list;
/* object make_package(); */
/* object in_package(); */
/* object rename_package(); */
object find_package();
/* object coerce_to_package(); */
object current_package();
object intern();
object find_symbol();

/*  pathname.d  */
EXTER object Vdefault_pathname_defaults;
EXTER object sKwild;
EXTER object sKnewest;
EXTER object sKstart;
EXTER object sKend;
EXTER object sKjunk_allowed;
EXTER object sKhost;
EXTER object sKdevice;
EXTER object sKdirectory;
EXTER object sKname;
EXTER object sKtype;
EXTER object sKversion;
EXTER object sKdefaults;

EXTER object sKabsolute;
EXTER object sKrelative;
EXTER object sKup;

/* object parse_namestring(); */
object coerce_to_pathname();
/* object default_device(); */
object merge_pathnames();
object namestring();
object coerce_to_namestring();
object coerce_to_pathname();
object file_error();
object wild_pathname_p();

extern int pathname_resolve();
extern object pathname_lookup();
extern object translate_logical_pathname();
extern object search_local_pathname();
extern object coerce_to_local_namestring();
extern object expand_pathname();

/*  print.d  */
EXTER object sKupcase;
EXTER object sKdowncase;
EXTER object sKpreserve;
EXTER object sKinvert;
EXTER object sKcapitalize;
EXTER object sKpreserve;
EXTER object sKinvert;
EXTER object sKstream;
EXTER object sKreadably;
EXTER object sKescape;
EXTER object sKpretty;
EXTER object sKcircle;
EXTER object sKbase;
EXTER object sKradix;
EXTER object sKcase;
EXTER object sKgensym;
EXTER object sKlevel;
EXTER object sKlength;
EXTER object sKarray;
EXTER object sKlinear;
EXTER object sKmiser;
EXTER object sKfill;
EXTER object sKmandatory;
EXTER object sKcurrent;
EXTER object sKblock;
EXTER object sLAprint_readablyA;
EXTER object sLAprint_escapeA;
EXTER object sLAprint_prettyA;
EXTER object sLAprint_circleA;
EXTER object sLAprint_baseA;
EXTER object sLAprint_radixA;
EXTER object sLAprint_caseA;
EXTER object sLAprint_gensymA;
EXTER object sLAprint_levelA;
EXTER object sLAprint_lengthA;
EXTER object sLAprint_arrayA;
EXTER object sSAprint_contextA;
EXTER object sSAprint_context_headA;
object princ();
object prin1();
object print();
object terpri();
EXTER object sSpretty_print_format;
EXTER int  line_length;

/*  Read.d  */
EXTER object standard_readtable;
EXTER object Vreadtable;
EXTER object sLAread_default_float_formatA;
EXTER object sLAread_baseA;
EXTER object sLAread_suppressA;
EXTER object READtable;
EXTER object read_byte1();
EXTER int READdefault_float_format;
EXTER int READbase;
EXTER bool READsuppress;
EXTER bool READeval;
EXTER object siSsharp_comma;
EXTER bool escape_flag;
EXTER object delimiting_char;
EXTER bool detect_eos_flag;
/* bool in_list_flag; */
EXTER bool dot_flag;
EXTER bool preserving_whitespace_flag;
EXTER object default_dispatch_macro;
EXTER object big_register_0;
EXTER int sharp_eq_context_max;
object read_char();
object read_char1(object,object);
object peek_char();
/* object read_object_recursive(); */
object read_object_non_recursive();
object standard_read_object_non_recursive();
object read_object();
/* object parse_number(); */
/* object parse_integer(); */
/* object copy_readtable(); */
/* object current_readtable(); */
/* object patch_sharp(); */
object read_fasl_vector();

/* fasdump.c */
EXTER object sharing_table;

/*  reference.c  */
object symbol_function();


/*  sequence.d  */
object alloc_simple_vector();
object alloc_simple_bitvector();
object alloc_vector();
object alloc_bitvector();
object elt();
object elt_set();
object reverse();
object nreverse();

/*  structure.c  */
EXTER object sSs_data;
object structure_ref();
object structure_set();
object structure_to_list();

/*  string.d  */
object alloc_simple_string();
object make_simple_string();
object copy_simple_string();
object alloc_string();
object make_string();
object copy_string();
object coerce_to_string();
EXTER int string_sign, string_boundary;

/*  symbol.d  */
EXTER object string_register;
/* EXTER object gensym_prefix; */
/* EXTER int gensym_counter; */
/* EXTER object sLgensym_counter; */
EXTER object gentemp_prefix;
EXTER int gentemp_counter;
EXTER object token;
object make_symbol();
object make_ordinary();
object make_special();
object make_constant();
object make_gmp_ordinary();
object make_si_ordinary();
object make_si_special();
object make_si_constant();
object make_keyword();
object symbol_value();
object symbol_name();
object getf();
object get();
object putf();
object putprop();
object fSsputprop();
object remprop();
object gensym();		/*  to be deleted  */

#ifdef UNIX
/*  unixsys.c  */
#else
/*  sys.c  */
#endif

#ifdef UNIX
/*  unixtime.c  */
object unix_time_to_universal_time();
#else
/*  time.c  */
#endif

/*  toplevel.c  */
EXTER object sLspecial,sLdeclare;
EXTER object sSvariable_documentation;
EXTER object sSfunction_documentation;
EXTER object sSsetf_function;
#define setf_fn_form(a_) (consp(a_) && MMcar(a_)==sLsetf &&\
                          consp(MMcdr(a_)) && type_of(MMcadr(a_))==t_symbol &&\
                          MMcddr(a_)==Cnil)

/*  typespec.c  */
EXTER object sLcommon,sLnull,sLcons,sLlist,siLproper_list,sLsymbol,sLarray,sLvector,sLbit_vector,sLstring;
EXTER object sLsequence,sLsimple_array,sLsimple_vector,sLsimple_bit_vector,sLsimple_string;
EXTER object sLcompiled_function,sLpathname,sLcharacter,sLnumber,sLrational,sLfloat;
EXTER object sLinteger,sLratio,sLshort_float,sLstandard_char;

EXTER object sLchar,sLnon_negative_char,sLnegative_char,sLsigned_char,sLunsigned_char;
EXTER object sLshort,sLnon_negative_short,sLnegative_short,sLsigned_short,sLunsigned_short;
EXTER object sLfixnum,sLnon_negative_fixnum,sLnegative_fixnum,sLsigned_fixnum,sLunsigned_fixnum;
EXTER object sLlfixnum,sLnon_negative_lfixnum,sLnegative_lfixnum;
EXTER object sLsigned_lfixnum,sLunsigned_lfixnum,sLnegative_bignum,sLnon_negative_bignum,sLbase_char;

EXTER object sLsigned_int,sLnon_negative_int,sLnegative_int,sLunsigned_int;

EXTER object sLseqind,sLrnkind;

EXTER object sLcomplex;
EXTER object sLsingle_float,sLpackage,sLbignum,sLrandom_state,sLdouble_float,sLstream,sLbit,sLreadtable;
EXTER object sLlong_float,sLhash_table,sLstructure,sLboolean,sLfile_stream,sLinput_stream,sLoutput_stream,sLtype_error;
EXTER object sLbroadcast_stream,sLconcatenated_stream,sLecho_stream,sLfile_stream,sLstring_stream;
EXTER object sLsynonym_stream,sLtwo_way_stream;


EXTER object sLsatisfies;
EXTER object sLmember;
EXTER object sLnot;
EXTER object sLor;
EXTER object sLand;
EXTER object sLvalues;
EXTER object sLmod;
EXTER object sLsigned_byte;
EXTER object sLunsigned_byte;
EXTER object sSsigned_char;
EXTER object sSunsigned_char;
EXTER object sSsigned_short;
EXTER object sSunsigned_short;
EXTER object sLA;
EXTER object sLplusp;
EXTER object TSor_symbol_string;
EXTER object TSor_string_symbol;
EXTER object TSor_symbol_string_package;
EXTER object TSnon_negative_integer;
EXTER object TSpositive_number;
EXTER object TSor_integer_float;
EXTER object TSor_rational_float;
#ifdef UNIX
EXTER object TSor_pathname_string_symbol;
#endif
EXTER object TSor_pathname_string_symbol_stream;

EXTER int interrupt_flag;		/* console interupt flag */
EXTER int interrupt_enable;		/* console interupt enable */

EXTER object sSAlink_arrayA;

/* nfunlink.c */
object Icall_proc();
float Icall_proc_float();
/* object Icall_proc(); */
float Icall_proc_float();
object ImakeStructure();
object list_vector();
object list_vector_new();
object Iapply_ap();
object IisFboundp();
object IapplyVector();
EXTER object sSPmemory;
EXTER object sSPinit;
object sLfset();
object MakeAfun();
extern object call_proc0();
/* extern object call_proc(); */
/* extern object call_vproc(); */
object fLrow_major_aref();
object Icheck_one_type();

/* utils.c */
object Iis_fixnum();
object Iapply_fun_n(object,int,int,...);
object Iapply_fun_n1(object (*)(),int,int,...);
object Iapply_fun_n2(object,int,int,...);
object Ifuncall_n(object,int,...);
object Ivs_values();
object Icheck_one_type();
object fSincorrect_type();
