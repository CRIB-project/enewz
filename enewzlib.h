/*======================================================================*/
#ifndef ENEWZLIB_H
/*======================================================================*/

#define ENEWZLIB_H

PROTOCCALLSFFUN7(FLOAT,ENERGYNEWZ,energynewz,
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT,FLOAT)
#define ENERGYNEWZ(A2,A3,A4,A5,A6,A7,A8)\
     CCALLSFFUN7(ENERGYNEWZ,energynewz,\
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT,FLOAT,\
		 A2,A3,A4,A5,A6,A7,A8)
PROTOCCALLSFFUN7(FLOAT,ENERGYOLDZ,energyoldz,
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT,FLOAT)
#define ENERGYOLDZ(A2,A3,A4,A5,A6,A7,A8)\
     CCALLSFFUN7(ENERGYOLDZ,energyoldz,\
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT,FLOAT,\
		 A2,A3,A4,A5,A6,A7,A8)
PROTOCCALLSFFUN6(FLOAT,DEDXZ,dedxz,
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT)
#define DEDXZ(A2,A3,A4,A5,A6,A7)\
     CCALLSFFUN6(DEDXZ,dedxz,\
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT,\
		 A2,A3,A4,A5,A6,A7)
PROTOCCALLSFFUN6(FLOAT,E2RANGEZ,e2rangez,
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT)
#define E2RANGEZ(A2,A3,A4,A5,A6,A7)\
     CCALLSFFUN6(E2RANGEZ,e2rangez,\
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT,\
		 A2,A3,A4,A5,A6,A7)
PROTOCCALLSFFUN6(FLOAT,R2ENERGYZ,r2energyz,
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT)
#define R2ENERGYZ(A2,A3,A4,A5,A6,A7)\
     CCALLSFFUN6(R2ENERGYZ,r2energyz,\
		 INT,FLOAT,INT,INTV,FLOATV,FLOAT,\
		 A2,A3,A4,A5,A6,A7)
#define STOP(A1,A2,A3,A4,A5,A6,A7)\
     CCALLSFSUB7(STOP,stop,\
		 INT,FLOAT,INT,FLOAT,PFLOAT,PFLOAT,INT,\
		 A1,A2,A3,A4,A5,A6,A7)
#define SCOEF(A1,A2,A3,A4,A5,A6,A7,A8,A9)\
     CCALLSFSUB9(SCOEF,scoef,\
		 INT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,FLOATV,\
		 A1,A2,A3,A4,A5,A6,A7,A8,A9)
#define SNKEMATTER(A1,A2,A3,A4,A5,A6,A7)\
     CCALLSFSUB7(SNKEMATTER,snkematter,\
		 STRING,INT,PINT,INTV,FLOATV,PFLOAT,PFLOAT,\
		 A1,A2,A3,A4,A5,A6,A7)
#define SRHOGAS(A1,A2,A3,A4,A5,A6)\
     CCALLSFSUB6(SRHOGAS,srhogas,\
		 FLOAT,FLOAT,INT,INT,FLOAT,PFLOAT,\
		 A1,A2,A3,A4,A5,A6)
#define CHANGECASE(A1,A2)\
     CCALLSFSUB2(CHANGECASE,changecase,\
		 PSTRING,INT,\
		 A1,A2)

/*======================================================================*/
#endif
/*======================================================================*/
