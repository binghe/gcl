
#include "cmpinclude.h"
#include "gcl_defpackage.h"
void init_gcl_defpackage(){do_init(VV);}
/*	macro definition for DEFPACKAGE	*/

static void L1()
{register object *base=vs_base;
	register object *sup=base+VM1; VC1
	vs_check;
	vs_top=sup;
	{object V1=base[0]->c.c_cdr;
	base[2]= (V1->c.c_car);
	V1=V1->c.c_cdr;
	base[3]= V1;}
	{register object V2;
	register object V3;
	V2= Cnil;
	V3= base[3];
	goto T3;
T3:;
	if(((V3))==Cnil){
	goto T6;}
	goto T5;
	goto T6;
T6:;
	goto T4;
	goto T5;
T5:;
	{register object V4;
	V4= CMPcar((V3));
	V2= (V4);}
	{register object x= CMPcar((V2)),V5= VV[0];
	while(V5!=Cnil)
	if(eql(x,V5->c.c_car)){
	goto T11;
	}else V5=V5->c.c_cdr;}
	base[4]= VV[1];
	base[5]= VV[2];
	base[6]= (V2);
	vs_top=(vs_base=base+4)+3;
	Lcerror();
	vs_top=sup;
	goto T11;
T11:;
	V3= CMPcdr((V3));
	goto T3;
	goto T4;
T4:;
	goto T1;
	goto T1;}
	goto T1;
T1:;
	base[4]=MMcons(Cnil,Cnil);
	(base[4]->c.c_car)=make_cclosure_new(LC3,Cnil,base[4],Cdata);
	{register object V6;
	register object V7;
	V6= Cnil;
	V7= VV[3];
	goto T22;
T22:;
	if(((V7))==Cnil){
	goto T25;}
	goto T24;
	goto T25;
T25:;
	goto T23;
	goto T24;
T24:;
	{register object V8;
	V8= CMPcar((V7));
	V6= (V8);}
	base[5]= (V6);
	base[6]= base[3];
	base[7]= VV[4];
	base[8]= (VV[70]->s.s_gfdef);
	vs_top=(vs_base=base+5)+4;
	(void) (*Lnk71)();
	vs_top=sup;
	V9= vs_base[0];
	if(!(number_compare(small_fixnum(2),V9)<=0)){
	goto T30;}
	base[5]= VV[5];
	base[6]= VV[6];
	base[7]= (V6);
	vs_top=(vs_base=base+5)+3;
	(void) (*Lnk72)();
	vs_top=sup;
	goto T30;
T30:;
	V7= CMPcdr((V7));
	goto T22;
	goto T23;
T23:;
	goto T20;
	goto T20;}
	goto T20;
T20:;
	base[2]= coerce_to_string(base[2]);
	{object V10;
	object V11;
	object V12;
	register object V13;
	register object V14;
	register object V15;
	register object V16;
	register object V17;
	object V18;
	base[6]= VV[7];
	base[7]= base[3];
	vs_top=(vs_base=base+6)+2;
	L5(base);
	vs_top=sup;
	V21= vs_base[0];
	{object V19;
	object V20= V21;
	if(V20==Cnil){
	V10= Cnil;
	goto T46;}
	base[5]=V19=MMcons(Cnil,Cnil);
	goto T47;
T47:;
	(V19->c.c_car)= coerce_to_string((V20->c.c_car));
	if((V20=MMcdr(V20))==Cnil){
	V10= base[5];
	goto T46;}
	V19=MMcdr(V19)=MMcons(Cnil,Cnil);
	goto T47;}
	goto T46;
T46:;
	base[5]= VV[8];
	base[6]= base[3];
	vs_top=(vs_base=base+5)+2;
	L5(base);
	vs_top=sup;
	V22= vs_base[0];
	V11= CMPcar(V22);
	base[5]= VV[9];
	base[6]= base[3];
	vs_top=(vs_base=base+5)+2;
	L5(base);
	vs_top=sup;
	V23= vs_base[0];
	V12= CMPcar(V23);
	base[6]= VV[10];
	base[7]= base[3];
	vs_top=(vs_base=base+6)+2;
	L5(base);
	vs_top=sup;
	V26= vs_base[0];
	{object V24;
	object V25= V26;
	if(V25==Cnil){
	V13= Cnil;
	goto T60;}
	base[5]=V24=MMcons(Cnil,Cnil);
	goto T61;
T61:;
	(V24->c.c_car)= coerce_to_string((V25->c.c_car));
	if((V25=MMcdr(V25))==Cnil){
	V13= base[5];
	goto T60;}
	V24=MMcdr(V24)=MMcons(Cnil,Cnil);
	goto T61;}
	goto T60;
T60:;
	base[6]= VV[11];
	base[7]= base[3];
	vs_top=(vs_base=base+6)+2;
	L5(base);
	vs_top=sup;
	V29= vs_base[0];
	{object V27;
	object V28= V29;
	if(V28==Cnil){
	V14= Cnil;
	goto T66;}
	base[5]=V27=MMcons(Cnil,Cnil);
	goto T67;
T67:;
	(V27->c.c_car)= coerce_to_string((V28->c.c_car));
	if((V28=MMcdr(V28))==Cnil){
	V14= base[5];
	goto T66;}
	V27=MMcdr(V27)=MMcons(Cnil,Cnil);
	goto T67;}
	goto T66;
T66:;
	base[6]= VV[12];
	base[7]= base[3];
	vs_top=(vs_base=base+6)+2;
	L5(base);
	vs_top=sup;
	V32= vs_base[0];
	{object V30;
	object V31= V32;
	if(V31==Cnil){
	V15= Cnil;
	goto T72;}
	base[5]=V30=MMcons(Cnil,Cnil);
	goto T73;
T73:;
	(V30->c.c_car)= coerce_to_string((V31->c.c_car));
	if((V31=MMcdr(V31))==Cnil){
	V15= base[5];
	goto T72;}
	V30=MMcdr(V30)=MMcons(Cnil,Cnil);
	goto T73;}
	goto T72;
T72:;
	{register object V33;
	register object V34;
	base[5]= VV[13];
	base[6]= base[3];
	vs_top=(vs_base=base+5)+2;
	L4(base);
	vs_top=sup;
	V34= vs_base[0];
	V33= Cnil;
	{register object V35;
	register object V36;
	register object V37;
	V35= Cnil;
	V36= Cnil;
	V37= Cnil;
	goto T83;
T83:;
	if(((V34))==Cnil){
	goto T86;}
	goto T85;
	goto T86;
T86:;
	goto T84;
	goto T85;
T85:;
	{register object V38;
	V38= CMPcar((V34));
	V33= (V38);}
	V39= coerce_to_string(CMPcar((V33)));
	{object V41;
	object V42= CMPcdr((V33));
	if(V42==Cnil){
	V40= Cnil;
	goto T93;}
	base[5]=V41=MMcons(Cnil,Cnil);
	goto T94;
T94:;
	(V41->c.c_car)= coerce_to_string((V42->c.c_car));
	if((V42=MMcdr(V42))==Cnil){
	V40= base[5];
	goto T93;}
	V41=MMcdr(V41)=MMcons(Cnil,Cnil);
	goto T94;}
	goto T93;
T93:;
	V37= make_cons(/* INLINE-ARGS */V39,V40);
	if(((V36))==Cnil){
	goto T98;}
	{register object V43;
	register object V44;
	V43= (V36);
	V36= make_cons((V37),Cnil);
	V44= (V36);
	((V43))->c.c_cdr = (V44);
	goto T96;}
	goto T98;
T98:;
	V36= make_cons((V37),Cnil);
	V35= (V36);
	goto T96;
T96:;
	V34= CMPcdr((V34));
	goto T83;
	goto T84;
T84:;
	V16= (V35);
	goto T78;
	V16= Cnil;
	goto T78;
	V16= Cnil;
	goto T78;}}
	goto T78;
T78:;
	{register object V45;
	register object V46;
	base[5]= VV[14];
	base[6]= base[3];
	vs_top=(vs_base=base+5)+2;
	L4(base);
	vs_top=sup;
	V46= vs_base[0];
	V45= Cnil;
	{register object V47;
	register object V48;
	register object V49;
	V47= Cnil;
	V48= Cnil;
	V49= Cnil;
	goto T115;
T115:;
	if(((V46))==Cnil){
	goto T118;}
	goto T117;
	goto T118;
T118:;
	goto T116;
	goto T117;
T117:;
	{register object V50;
	V50= CMPcar((V46));
	V45= (V50);}
	V51= coerce_to_string(CMPcar((V45)));
	{object V53;
	object V54= CMPcdr((V45));
	if(V54==Cnil){
	V52= Cnil;
	goto T125;}
	base[5]=V53=MMcons(Cnil,Cnil);
	goto T126;
T126:;
	(V53->c.c_car)= coerce_to_string((V54->c.c_car));
	if((V54=MMcdr(V54))==Cnil){
	V52= base[5];
	goto T125;}
	V53=MMcdr(V53)=MMcons(Cnil,Cnil);
	goto T126;}
	goto T125;
T125:;
	V49= make_cons(/* INLINE-ARGS */V51,V52);
	if(((V48))==Cnil){
	goto T130;}
	{register object V55;
	register object V56;
	V55= (V48);
	V48= make_cons((V49),Cnil);
	V56= (V48);
	((V55))->c.c_cdr = (V56);
	goto T128;}
	goto T130;
T130:;
	V48= make_cons((V49),Cnil);
	V47= (V48);
	goto T128;
T128:;
	V46= CMPcdr((V46));
	goto T115;
	goto T116;
T116:;
	V17= (V47);
	goto T110;
	V17= Cnil;
	goto T110;
	V17= Cnil;
	goto T110;}}
	goto T110;
T110:;
	base[6]= VV[15];
	base[7]= base[3];
	vs_top=(vs_base=base+6)+2;
	L5(base);
	vs_top=sup;
	V59= vs_base[0];
	{object V57;
	object V58= V59;
	if(V58==Cnil){
	V18= Cnil;
	goto T142;}
	base[5]=V57=MMcons(Cnil,Cnil);
	goto T143;
T143:;
	(V57->c.c_car)= coerce_to_string((V58->c.c_car));
	if((V58=MMcdr(V58))==Cnil){
	V18= base[5];
	goto T142;}
	V57=MMcdr(V57)=MMcons(Cnil,Cnil);
	goto T143;}
	goto T142;
T142:;
	{register object V60;
	register object V61;
	base[5]= (V13);
	base[6]= (V14);
	{register object V62;
	register object V63;
	V62= Cnil;
	V63= (V16);
	{register object V64;
	register object V65;
	register object V66;
	V64= Cnil;
	V65= Cnil;
	V66= Cnil;
	goto T155;
T155:;
	if(((V63))==Cnil){
	goto T158;}
	goto T157;
	goto T158;
T158:;
	goto T156;
	goto T157;
T157:;
	{register object V67;
	V67= CMPcar((V63));
	V62= (V67);}
	base[8]= CMPcdr((V62));
	vs_top=(vs_base=base+8)+1;
	Lcopy_list();
	vs_top=sup;
	V66= vs_base[0];
	if(((V65))==Cnil){
	goto T168;}
	{register object V68;
	register object V69;
	V68= (V65);
	V69= (V66);
	((V68))->c.c_cdr = (V69);
	if(((V69))!=Cnil){
	goto T170;}}
	goto T166;
	goto T170;
T170:;
	base[8]= CMPcdr((V65));
	vs_top=(vs_base=base+8)+1;
	Llast();
	vs_top=sup;
	V65= vs_base[0];
	goto T166;
	goto T168;
T168:;
	V64= (V66);
	base[8]= (V64);
	vs_top=(vs_base=base+8)+1;
	Llast();
	vs_top=sup;
	V65= vs_base[0];
	goto T166;
T166:;
	V63= CMPcdr((V63));
	goto T155;
	goto T156;
T156:;
	base[7]= (V64);
	goto T153;
	base[7]= Cnil;
	goto T153;
	base[7]= Cnil;
	goto T153;}}
	goto T153;
T153:;
	{register object V70;
	register object V71;
	V70= Cnil;
	V71= (V17);
	{register object V72;
	register object V73;
	register object V74;
	V72= Cnil;
	V73= Cnil;
	V74= Cnil;
	goto T186;
T186:;
	if(((V71))==Cnil){
	goto T189;}
	goto T188;
	goto T189;
T189:;
	goto T187;
	goto T188;
T188:;
	{register object V75;
	V75= CMPcar((V71));
	V70= (V75);}
	base[9]= CMPcdr((V70));
	vs_top=(vs_base=base+9)+1;
	Lcopy_list();
	vs_top=sup;
	V74= vs_base[0];
	if(((V73))==Cnil){
	goto T199;}
	{register object V76;
	register object V77;
	V76= (V73);
	V77= (V74);
	((V76))->c.c_cdr = (V77);
	if(((V77))!=Cnil){
	goto T201;}}
	goto T197;
	goto T201;
T201:;
	base[9]= CMPcdr((V73));
	vs_top=(vs_base=base+9)+1;
	Llast();
	vs_top=sup;
	V73= vs_base[0];
	goto T197;
	goto T199;
T199:;
	V72= (V74);
	base[9]= (V72);
	vs_top=(vs_base=base+9)+1;
	Llast();
	vs_top=sup;
	V73= vs_base[0];
	goto T197;
T197:;
	V71= CMPcdr((V71));
	goto T186;
	goto T187;
T187:;
	base[8]= (V72);
	goto T184;
	base[8]= Cnil;
	goto T184;
	base[8]= Cnil;
	goto T184;}}
	goto T184;
T184:;
	vs_top=(vs_base=base+5)+4;
	L6(base);
	vs_top=sup;
	V61= vs_base[0];
	V60= Cnil;
	goto T216;
T216:;
	if(((V61))==Cnil){
	goto T219;}
	goto T218;
	goto T219;
T219:;
	goto T217;
	goto T218;
T218:;
	{register object V78;
	V78= CMPcar((V61));
	V60= (V78);}
	base[5]= VV[5];
	base[6]= VV[16];
	base[7]= CMPcar((V60));
	{register object V79;
	register object V80;
	V80= CMPcdr((V60));
	V79= Cnil;
	{register object V81;
	register object V82;
	register object V83;
	V81= Cnil;
	V82= Cnil;
	V83= Cnil;
	goto T231;
T231:;
	if(((V80))==Cnil){
	goto T234;}
	goto T233;
	goto T234;
T234:;
	goto T232;
	goto T233;
T233:;
	{register object V84;
	V84= CMPcar((V80));
	V79= (V84);}
	{object V85= (V79);
	if(!eql(V85,VV[17]))goto T241;
	V83= VV[10];
	goto T240;
	goto T241;
T241:;
	if(!eql(V85,VV[18]))goto T242;
	V83= VV[11];
	goto T240;
	goto T242;
T242:;
	if(!eql(V85,VV[19]))goto T243;
	V83= VV[13];
	goto T240;
	goto T243;
T243:;
	if(!eql(V85,VV[20]))goto T244;
	V83= VV[14];
	goto T240;
	goto T244;
T244:;
	V83= Cnil;}
	goto T240;
T240:;
	if(((V82))==Cnil){
	goto T247;}
	{register object V86;
	register object V87;
	V86= (V82);
	V82= make_cons((V83),Cnil);
	V87= (V82);
	((V86))->c.c_cdr = (V87);
	goto T245;}
	goto T247;
T247:;
	V82= make_cons((V83),Cnil);
	V81= (V82);
	goto T245;
T245:;
	V80= CMPcdr((V80));
	goto T231;
	goto T232;
T232:;
	base[8]= (V81);
	goto T228;
	base[8]= Cnil;
	goto T228;
	base[8]= Cnil;
	goto T228;}}
	goto T228;
T228:;
	vs_top=(vs_base=base+5)+4;
	(void) (*Lnk72)();
	vs_top=sup;
	V61= CMPcdr((V61));
	goto T216;
	goto T217;
T217:;
	goto T149;
	goto T149;}
	goto T149;
T149:;
	{register object V88;
	register object V89;
	base[5]= (V15);
	base[6]= (V14);
	vs_top=(vs_base=base+5)+2;
	L6(base);
	vs_top=sup;
	V89= vs_base[0];
	V88= Cnil;
	goto T266;
T266:;
	if(((V89))==Cnil){
	goto T269;}
	goto T268;
	goto T269;
T269:;
	goto T267;
	goto T268;
T268:;
	{register object V90;
	V90= CMPcar((V89));
	V88= (V90);}
	base[5]= VV[5];
	base[6]= VV[21];
	base[7]= CMPcar((V88));
	{register object V91;
	register object V92;
	V92= CMPcdr((V88));
	V91= Cnil;
	{register object V93;
	register object V94;
	register object V95;
	V93= Cnil;
	V94= Cnil;
	V95= Cnil;
	goto T281;
T281:;
	if(((V92))==Cnil){
	goto T284;}
	goto T283;
	goto T284;
T284:;
	goto T282;
	goto T283;
T283:;
	{register object V96;
	V96= CMPcar((V92));
	V91= (V96);}
	{object V97= (V91);
	if(!eql(V97,VV[17]))goto T291;
	V95= VV[12];
	goto T290;
	goto T291;
T291:;
	if(!eql(V97,VV[18]))goto T292;
	V95= VV[11];
	goto T290;
	goto T292;
T292:;
	V95= Cnil;}
	goto T290;
T290:;
	if(((V94))==Cnil){
	goto T295;}
	{register object V98;
	register object V99;
	V98= (V94);
	V94= make_cons((V95),Cnil);
	V99= (V94);
	((V98))->c.c_cdr = (V99);
	goto T293;}
	goto T295;
T295:;
	V94= make_cons((V95),Cnil);
	V93= (V94);
	goto T293;
T293:;
	V92= CMPcdr((V92));
	goto T281;
	goto T282;
T282:;
	base[8]= (V93);
	goto T278;
	base[8]= Cnil;
	goto T278;
	base[8]= Cnil;
	goto T278;}}
	goto T278;
T278:;
	vs_top=(vs_base=base+5)+4;
	(void) (*Lnk72)();
	vs_top=sup;
	V89= CMPcdr((V89));
	goto T266;
	goto T267;
T267:;
	goto T148;
	goto T148;}
	goto T148;
T148:;
	V100= list(2,VV[27],base[2]);
	V101= list(3,VV[29],base[2],base[2]);
	if(((V10))==Cnil){
	goto T312;}
	V103= list(4,VV[29],base[2],base[2],list(2,VV[30],(V10)));
	V102= make_cons(/* INLINE-ARGS */V103,Cnil);
	goto T310;
	goto T312;
T312:;
	V102= Cnil;
	goto T310;
T310:;
	base[5]= VV[31];
	base[6]= base[3];
	base[7]= VV[4];
	base[8]= (VV[70]->s.s_gfdef);
	vs_top=(vs_base=base+5)+4;
	Lmember();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T316;}
	V105= list(3,VV[32],list(2,VV[33],list(2,VV[27],base[2])),base[2]);
	V104= make_cons(/* INLINE-ARGS */V105,Cnil);
	goto T314;
	goto T316;
T316:;
	V104= Cnil;
	goto T314;
T314:;
	V106= listA(3,VV[28],/* INLINE-ARGS */V101,append(V102,V104));
	V107= list(2,VV[30],(V10));
	if(((V12))==Cnil){
	goto T324;}
	V108= Cnil;
	goto T322;
	goto T324;
T324:;
	V108= Cnil;
	goto T322;
T322:;
	V109= list(4,VV[26],/* INLINE-ARGS */V100,/* INLINE-ARGS */V106,listA(7,VV[34],base[2],VV[35],VV[36],VV[37],/* INLINE-ARGS */V107,V108));
	base[5]= base[2];
	base[6]= VV[40];
	vs_top=(vs_base=base+5)+2;
	Lintern();
	vs_top=sup;
	V110= vs_base[0];
	V111= list(3,VV[38],list(3,VV[39],list(2,VV[30],V110),VV[41]),(V11));
	V112= make_cons(/* INLINE-ARGS */V111,Cnil);
	V113= list(2,VV[43],list(2,VV[27],base[2]));
	V114= make_cons(/* INLINE-ARGS */V113,Cnil);
	if(((V13))==Cnil){
	goto T332;}
	V116= list(2,VV[44],list(3,VV[45],VV[46],list(2,VV[30],(V13))));
	base[5]= make_cons(/* INLINE-ARGS */V116,Cnil);
	goto T330;
	goto T332;
T332:;
	base[5]= Cnil;
	goto T330;
T330:;
	if(((V16))==Cnil){
	goto T336;}
	{object V117;
	object V118= (V16);
	if(V118==Cnil){
	base[6]= Cnil;
	goto T334;}
	base[7]=V117=MMcons(Cnil,Cnil);
	goto T338;
T338:;
	{register object V119;
	V119= (V118->c.c_car);
	V120= list(3,VV[51],VV[52],CMPcar((V119)));
	V121= list(3,VV[53],VV[52],CMPcar((V119)));
	V122= list(2,VV[48],list(3,VV[49],VV[50],list(4,VV[26],/* INLINE-ARGS */V120,/* INLINE-ARGS */V121,list(5,VV[54],VV[55],VV[56],CMPcar((V119)),VV[57]))));
	(V117->c.c_car)= list(2,VV[47],list(3,VV[45],/* INLINE-ARGS */V122,list(2,VV[30],CMPcdr((V119)))));}
	if((V118=MMcdr(V118))==Cnil){
	base[6]= base[7];
	goto T334;}
	V117=MMcdr(V117)=MMcons(Cnil,Cnil);
	goto T338;}
	goto T336;
T336:;
	base[6]= Cnil;
	goto T334;
T334:;
	base[8]= VV[31];
	base[9]= base[3];
	base[10]= VV[22];
	base[11]= (base[4]->c.c_car);
	vs_top=(vs_base=base+8)+4;
	Lmember();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T343;}
	base[9]= VV[31];
	base[10]= base[3];
	vs_top=(vs_base=base+9)+2;
	L5(base);
	vs_top=sup;
	V126= vs_base[0];
	{object V124;
	object V125= V126;
	if(V125==Cnil){
	V123= Cnil;
	goto T341;}
	base[8]=V124=MMcons(Cnil,Cnil);
	goto T349;
T349:;
	(V124->c.c_car)= coerce_to_string((V125->c.c_car));
	if((V125=MMcdr(V125))==Cnil){
	V123= base[8];
	goto T341;}
	V124=MMcdr(V124)=MMcons(Cnil,Cnil);
	goto T349;}
	goto T343;
T343:;
	V123= VV[59];
	goto T341;
T341:;
	V127= list(2,VV[58],list(2,VV[30],V123));
	if(((V17))==Cnil){
	goto T357;}
	{object V129;
	object V130= (V17);
	if(V130==Cnil){
	base[8]= Cnil;
	goto T355;}
	base[9]=V129=MMcons(Cnil,Cnil);
	goto T359;
T359:;
	{register object V131;
	V131= (V130->c.c_car);
	V132= list(3,VV[51],VV[52],CMPcar((V131)));
	V133= list(3,VV[53],VV[52],CMPcar((V131)));
	V134= list(2,VV[48],list(3,VV[49],VV[61],list(4,VV[26],/* INLINE-ARGS */V132,/* INLINE-ARGS */V133,list(5,VV[54],VV[55],VV[62],CMPcar((V131)),VV[63]))));
	(V129->c.c_car)= list(2,VV[60],list(3,VV[45],/* INLINE-ARGS */V134,list(2,VV[30],CMPcdr((V131)))));}
	if((V130=MMcdr(V130))==Cnil){
	base[8]= base[9];
	goto T355;}
	V129=MMcdr(V129)=MMcons(Cnil,Cnil);
	goto T359;}
	goto T357;
T357:;
	base[8]= Cnil;
	goto T355;
T355:;
	if(((V14))==Cnil){
	goto T363;}
	V135= list(3,VV[45],VV[64],list(2,VV[30],(V14)));
	base[9]= make_cons(/* INLINE-ARGS */V135,Cnil);
	goto T361;
	goto T363;
T363:;
	base[9]= Cnil;
	goto T361;
T361:;
	if(((V15))==Cnil){
	goto T367;}
	V136= list(2,VV[65],list(3,VV[45],VV[66],list(2,VV[30],(V15))));
	base[10]= make_cons(/* INLINE-ARGS */V136,Cnil);
	goto T365;
	goto T367;
T367:;
	base[10]= Cnil;
	goto T365;
T365:;
	if(((V18))==Cnil){
	goto T371;}
	V137= list(3,VV[67],list(2,VV[68],list(2,VV[30],(V18))),VV[69]);
	base[11]= make_cons(/* INLINE-ARGS */V137,Cnil);
	goto T369;
	goto T371;
T371:;
	base[11]= Cnil;
	goto T369;
T369:;
	vs_top=(vs_base=base+8)+4;
	Lappend();
	vs_top=sup;
	V128= vs_base[0];
	base[7]= make_cons(/* INLINE-ARGS */V127,V128);
	vs_top=(vs_base=base+5)+3;
	Lappend();
	vs_top=sup;
	V115= vs_base[0];
	V138= listA(3,VV[42],/* INLINE-ARGS */V114,V115);
	V139= list(2,/* INLINE-ARGS */V138,list(2,VV[27],base[2]));
	base[5]= listA(4,VV[24],VV[25],/* INLINE-ARGS */V109,append(/* INLINE-ARGS */V112,/* INLINE-ARGS */V139));
	vs_top=(vs_base=base+5)+1;
	return;}
}
/*	local function FIND-DUPLICATES	*/

static void L6(base0)
register object *base0;
{	register object *base=vs_base;
	register object *sup=base+VM2; VC2
	vs_check;
	{object V140;
	vs_top[0]=Cnil;
	{object *p=vs_top;
	 for(;p>vs_base;p--)p[-1]=MMcons(p[-1],p[0]);}
	V140=(base[0]);
	vs_top=sup;
	{object V141;
	V141= Cnil;
	{object V142;
	object V143;
	V142= Cnil;
	V143= (V140);
	{object V144;
	V144= CMPcdr((V140));
	{register long V145;
	V145= 1;
	goto T377;
T377:;
	if(((V144))==Cnil){
	goto T380;}
	goto T379;
	goto T380;
T380:;
	goto T378;
	goto T379;
T379:;
	if(((V143))==Cnil){
	goto T383;}
	goto T382;
	goto T383;
T383:;
	goto T378;
	goto T382;
T382:;
	{object V146;
	V146= CMPcar((V143));
	V142= (V146);}
	{register object V147;
	register object V148;
	V147= Cnil;
	V148= (V142);
	{register object V149;
	V149= Cnil;
	goto T390;
T390:;
	if(((V148))==Cnil){
	goto T393;}
	goto T392;
	goto T393;
T393:;
	goto T391;
	goto T392;
T392:;
	{object V150;
	V150= CMPcar((V148));
	V147= (V150);}
	{object V151;
	base[1]= (V147);
	base[2]= (V141);
	base[3]= VV[4];
	base[4]= (VV[70]->s.s_gfdef);
	base[5]= VV[22];
	base[6]= (VV[73]->s.s_gfdef);
	vs_top=(vs_base=base+1)+6;
	(void) (*Lnk74)();
	vs_top=sup;
	V151= vs_base[0];
	V149= (V151);}
	V153 = CMPmake_fixnum(V145);
	{register object x= V153,V152= (V149);
	while(V152!=Cnil)
	if(eql(x,V152->c.c_car)){
	goto T407;
	}else V152=V152->c.c_cdr;}
	{object V154;
	register object V155;
	V154= Cnil;
	V155= (V144);
	{register long V156;
	{object V157;
	V158 = CMPmake_fixnum(V145);
	V157= one_plus(V158);{object V159;
	V159= (type_of((V157))==t_fixnum?Ct:Cnil);
	if(V159==Cnil)goto T413;
	goto T412;
	goto T413;
T413:;}
	vs_base=vs_top;
	(void) (*Lnk75)();
	vs_top=sup;
	goto T412;
T412:;
	V156= fix((V157));}
	goto T416;
T416:;
	if(((V155))==Cnil){
	goto T419;}
	goto T418;
	goto T419;
T419:;
	goto T417;
	goto T418;
T418:;
	{object V160;
	V160= CMPcar((V155));
	V154= (V160);}
	base[1]= (V147);
	base[2]= (V154);
	base[3]= VV[22];
	base[4]= (VV[73]->s.s_gfdef);
	vs_top=(vs_base=base+1)+4;
	Lmember();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T424;}
	if(((V149))==Cnil){
	goto T432;}
	V161 = CMPmake_fixnum(V156);
	V162= make_cons(V161,Cnil);
	(void)(nconc((V149),/* INLINE-ARGS */V162));
	goto T424;
	goto T432;
T432:;
	{object V164;
	V165 = CMPmake_fixnum(V145);
	V166 = CMPmake_fixnum(V156);
	V164= list(3,(V147),V165,V166);
	V141= make_cons((V164),(V141));
	V163= (V141);}
	V149= CMPcar(V163);
	goto T424;
T424:;
	V155= CMPcdr((V155));
	if(!((V156)<(2147483646))){
	goto T441;}
	goto T440;
	goto T441;
T441:;
	vs_base=vs_top;
	(void) (*Lnk75)();
	vs_top=sup;
	goto T440;
T440:;
	V156= (long)(V156)+(1);
	goto T416;
	goto T417;
T417:;
	goto T407;
	goto T407;}}
	goto T407;
T407:;
	V148= CMPcdr((V148));
	goto T390;
	goto T391;
T391:;
	goto T388;
	goto T388;}}
	goto T388;
T388:;
	V143= CMPcdr((V143));
	V144= CMPcdr((V144));
	if(!((V145)<(2147483646))){
	goto T454;}
	goto T453;
	goto T454;
T454:;
	vs_base=vs_top;
	(void) (*Lnk75)();
	vs_top=sup;
	goto T453;
T453:;
	V145= (long)(V145)+(1);
	goto T377;
	goto T378;
T378:;
	goto T373;
	goto T373;}}}
	goto T373;
T373:;
	base[1]= (V141);
	vs_top=(vs_base=base+1)+1;
	return;}
	}
}
/*	local function OPTION-TEST	*/

static void LC3(base0)
register object *base0;
{	register object *base=vs_base;
	register object *sup=base+VM3; VC3
	vs_check;
	{object V167;
	object V168;
	V167=(base[0]);
	V168=(base[1]);
	vs_top=sup;
	if(!(type_of((V168))==t_cons)){
	goto T460;}
	base[2]= (equal(CMPcar((V168)),(V167))?Ct:Cnil);
	vs_top=(vs_base=base+2)+1;
	return;
	goto T460;
T460:;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;
	}
}
/*	local function OPTION-VALUES	*/

static void L5(base0)
register object *base0;
{	register object *base=vs_base;
	register object *sup=base+VM4; VC4
	vs_check;
	{object V169;
	object V170;
	V169=(base[0]);
	V170=(base[1]);
	vs_top=sup;
	goto TTL;
TTL:;
	{object V171;
	V171= Cnil;
	{object V172;
	object V173;
	object V174;
	V172= Cnil;
	V173= Cnil;
	V174= Cnil;
	{object V175;
	base[2]= (V169);
	base[3]= (V170);
	base[4]= VV[22];
	base[5]= (base0[4]->c.c_car);
	vs_top=(vs_base=base+2)+4;
	Lmember();
	vs_top=sup;
	V175= vs_base[0];
	V171= (V175);}
	goto T463;
T463:;
	if(((V171))==Cnil){
	goto T472;}
	goto T471;
	goto T472;
T472:;
	goto T464;
	goto T471;
T471:;
	if(((V171))==Cnil){
	goto T474;}
	base[2]= CMPcdr(CMPcar((V171)));
	vs_top=(vs_base=base+2)+1;
	Lcopy_list();
	vs_top=sup;
	V174= vs_base[0];
	if(((V173))==Cnil){
	goto T481;}
	(V173)->c.c_cdr = V174;
	if((V174)!=Cnil){
	goto T483;}
	goto T474;
	goto T483;
T483:;
	base[2]= CMPcdr((V173));
	vs_top=(vs_base=base+2)+1;
	Llast();
	vs_top=sup;
	V173= vs_base[0];
	goto T474;
	goto T481;
T481:;
	V172= (V174);
	base[2]= (V172);
	vs_top=(vs_base=base+2)+1;
	Llast();
	vs_top=sup;
	V173= vs_base[0];
	goto T474;
T474:;
	{object V178;
	base[2]= (V169);
	base[3]= CMPcdr((V171));
	base[4]= VV[22];
	base[5]= (base0[4]->c.c_car);
	vs_top=(vs_base=base+2)+4;
	Lmember();
	vs_top=sup;
	V178= vs_base[0];
	V171= (V178);}
	goto T463;
	goto T464;
T464:;
	base[2]= (V172);
	vs_top=(vs_base=base+2)+1;
	return;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;}}
	}
}
/*	local function OPTION-VALUES-LIST	*/

static void L4(base0)
register object *base0;
{	register object *base=vs_base;
	register object *sup=base+VM5; VC5
	vs_check;
	{object V179;
	object V180;
	V179=(base[0]);
	V180=(base[1]);
	vs_top=sup;
	goto TTL;
TTL:;
	{object V181;
	V181= Cnil;
	{object V182;
	object V183;
	object V184;
	V182= Cnil;
	V183= Cnil;
	V184= Cnil;
	{object V185;
	base[2]= (V179);
	base[3]= (V180);
	base[4]= VV[22];
	base[5]= (base0[4]->c.c_car);
	vs_top=(vs_base=base+2)+4;
	Lmember();
	vs_top=sup;
	V185= vs_base[0];
	V181= (V185);}
	goto T501;
T501:;
	if(((V181))==Cnil){
	goto T510;}
	goto T509;
	goto T510;
T510:;
	goto T502;
	goto T509;
T509:;
	if(((V181))==Cnil){
	goto T512;}
	V184= CMPcdr(CMPcar((V181)));
	if(((V183))==Cnil){
	goto T518;}
	{object V186;
	object V187;
	V186= (V183);
	V183= make_cons((V184),Cnil);
	V187= (V183);
	((V186))->c.c_cdr = (V187);
	goto T512;}
	goto T518;
T518:;
	V183= make_cons((V184),Cnil);
	V182= (V183);
	goto T512;
T512:;
	{object V188;
	base[2]= (V179);
	base[3]= CMPcdr((V181));
	base[4]= VV[22];
	base[5]= (base0[4]->c.c_car);
	vs_top=(vs_base=base+2)+4;
	Lmember();
	vs_top=sup;
	V188= vs_base[0];
	V181= (V188);}
	goto T501;
	goto T502;
T502:;
	base[2]= (V182);
	vs_top=(vs_base=base+2)+1;
	return;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;}}
	}
}
static void LnkT75(){ call_or_link(VV[75],(void **)(void *)&Lnk75);} /* SLOOP-TYPE-ERROR */
static void LnkT74(){ call_or_link(VV[74],(void **)(void *)&Lnk74);} /* FIND */
static void LnkT72(){ call_or_link(VV[72],(void **)(void *)&Lnk72);} /* SPECIFIC-ERROR */
static void LnkT71(){ call_or_link(VV[71],(void **)(void *)&Lnk71);} /* COUNT */

#ifdef SYSTEM_SPECIAL_INIT
SYSTEM_SPECIAL_INIT
#endif
