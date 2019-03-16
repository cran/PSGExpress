#pragma once
#include <vector>
#include <cstddef>
#include <limits.h>
#ifdef PSG_EXPRESS
	#define MAXVARNUMBER 10;
#else
	#define MAXVARNUMBER INT_MAX;
#endif

#ifndef _SOLVER_CONST
#define _SOLVER_CONST
	const int max_name_len			= 128;   //!< maximum lenght of name in characters
	const int max_objectname_len	= 16512; //!< maximum lenght of object name in characters
	const double INFINITY_NEG = -1e20; //!< value assigned as "minus infinity"
	const double INFINITY_POS =  1e20; //!< value assigned as "plus infinity"
#endif

class IPSGSolverHelper
{
	IPSGSolverHelper( const IPSGSolverHelper &copy ) {m_pImpl=NULL;};
	IPSGSolverHelper &operator=( IPSGSolverHelper & );
public:
	IPSGSolverHelper(void);
	virtual ~IPSGSolverHelper(void);
	virtual int Init(void);
	virtual void End();
	bool IsInit();
	virtual int GetProblemDescription(char*& pBuffer) = 0;
	virtual int GetPoint(const char* szPointName, char*& pBuffer) = 0;
	virtual int  GetMatrixInfo(const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols) = 0;
	virtual int  GetMatrixInfoSp(const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols, int* coefs) = 0;
	virtual bool GetMatrixData(const char* lpszMatrixName, double*& pData,  int rows, int cols) = 0;
	virtual bool GetMatrixDataSp(const char* lpszMatrixName, double*& pData, int*& prows, int*& pcols, int coeffs) = 0;
	virtual void ReleaseBuffer(void* pBuffer) = 0;
	virtual void ReleaseMatrix(double* pMatrix) = 0;
	virtual bool SaveObjective(char szObjectiveName[max_objectname_len], double value) = 0;
	virtual bool SaveVars(char szVarsName[][max_name_len], double values[], unsigned nCount) = 0;
	virtual bool SaveVector(const char* szVectorName, double varvalues[], unsigned nCount) = 0;
	virtual bool SaveObjs(
							char szObjName[][max_objectname_len],
							double val[], 
							char szConstrName[][max_name_len],
							char type[],
							unsigned nCount 
						 ) = 0;
	virtual bool SaveConstraints(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount ) = 0;
	virtual bool SaveConstraintsSlack(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount ) = 0;
	virtual bool SaveStatus(const char* szStatus) = 0;
	virtual void OnMessage(int nMsgCode,  char* szMessage) = 0;
	virtual void OnError(int nErrCode, const char* lpszObject, const char* lpszDescription) = 0;
	virtual void OnWarning(int nWarnCode, const char* lpszObject, const char* lpszDescription) = 0;
	virtual int  OnCancel(void) = 0;
	virtual int OnFinish(void) = 0;
	virtual int OnCycleFinish(void) = 0;
	virtual void OnProcessStop(int nErrCode) = 0;
	virtual void GetRootPath(char* pBuffer) = 0;
	virtual int GetExternalFunctionInfo(const char* funcname, char*& pHeaderBuf, int *cols) {return -1;}
	virtual int RunExternalFunction(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double* funvalue) {return -1;}
	virtual int RunGradientExternalFunction(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double** gradvalue) {return -1;}
	virtual int GetLogParam(char* pBuffer) {return -1;};
	virtual int SetMultyProblem(int ismulty) {return 0;}
	virtual int SetTaskVerify(int verify) {return 0;}
	virtual int GetExternalFunctionInfoDir(const char* funcname, char*& pHeaderBuf, int* lenvars, int* lenscens) {return -1;}
	virtual int RunExternalFunctionDir(const char* funcname, char* pHeaderBuf, int lenvars, double* varvalues, int lenscens, int* numscens,
			 double*& probability, double*& funvalues) {return -1;}
	virtual int RunGradientExternalFunctionDir(const char* funcname, char* pHeaderBuf, int lenvars, double* varvalues, int lenscens, int* numscen,
			double*& probability, double*& gradvalue) {return -1;}
	virtual int SaveProblemName(char* problemname) = 0;
	virtual int SaveProblemStatement(char* problemstatement) = 0;
	virtual int SaveSolution(char* problemsolution) = 0;// {return 1;}
	virtual bool SaveMatrix(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow) = 0;
	virtual bool SavePoint(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount) = 0;
	virtual bool AddPoint(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount) = 0;
	virtual bool AddVector(const char* szVectorName, double varvalues[], unsigned nCount) = 0;
	virtual bool AddMatrix(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow) = 0;
private:
	class CImpl *m_pImpl;
};
