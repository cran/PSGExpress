#pragma once
#include "../../IPSGSolverHelper/src/IPSGSolverHelper.h"
#include "../../PSGShellObjects/src/PSGShellObjects.h"
#include <vector>
#include <string.h>
#include <vector>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <math.h>
#ifdef WIN32
#include <windows.h>
#elif LINUX
#include <dlfcn.h>
#else
#error Platform not supported
#endif
#define TEG_PROCESSSTOPNOW		12345  //!< stop now

enum WHATPRINT_TYPE_R {
		WHATPRINT_MESSAGE_R = 1,
		WHATPRINT_WARNING_R,
		WHATPRINT_ERROR_R,
		WHATPRINT_NOTHING_R
};

enum HELPER_REGIME_TYPE_R {
		HELPER_REGIME_UNKNOWN_R = 1,
		HELPER_REGIME_SOLVE_R,
		HELPER_REGIME_VERIFY_R,
		HELPER_REGIME_INTERRUPT_R,
		HELPER_REGIME_SOLVED_R
};

enum EXT_FUNTYPE {
	FUNTYPE_UNKNOWN = 1,
	FUNTYPE_REG,
	FUNTYPE_DIR
};

struct EXT_FUNCTION_DESCR_R
{
	EXT_FUNTYPE fntype;
	std::string fndescr;
	std::string fnname;
	std::string grname;
	std::vector<std::string> str_fn_args;
	std::vector<std::string> str_fn_vars;
	std::vector<void*> p_fn_args;
	int numarg;
	void* fnenv;
	int numscen;
	int fnruncount;
	int grruncount;
	void* hextlib;
	std::string lib_name;
	EXT_FUNCTION_DESCR_R(std::string thisdescr):fntype(FUNTYPE_UNKNOWN), fndescr(thisdescr), numarg(0), fnenv(NULL), numscen(0), fnruncount(0), grruncount(0), hextlib(NULL), lib_name(std::string("")) {;}
	EXT_FUNCTION_DESCR_R(std::string thisdescr, std::string thisfname, std::string thisgrname, EXT_FUNTYPE fntype_, std::vector<std::string> this_fn_args):
		fntype(fntype_), fndescr(thisdescr), fnname(thisfname), grname(thisgrname), str_fn_args(this_fn_args), fnenv(NULL), numscen(0), fnruncount(0), grruncount(0), hextlib(NULL), lib_name(std::string(""))
	{numarg = (int)this_fn_args.size(); str_fn_vars.resize(0);}
	void clearruncount(void) {fnruncount=0; grruncount=0;}
	bool operator==(const EXT_FUNCTION_DESCR_R &right) {return strcasecmp(fndescr.c_str(), right.fndescr.c_str())==0;}
	bool operator==(const std::string &right) {return strcasecmp(fndescr.c_str(), right.c_str())==0;}
};

namespace strutil_Helper {
	char *strtok_uni(char *str, const char *delim, char **saveptr);
    std::string trim(const std::string& str);
    std::string RtoLower(const std::string& str);
    std::string RtoUpper(const std::string& str);
    std::vector<std::string> split(const std::string& str, const std::string& delimiters, bool bKeepBlanks);
    char *chartrim(char *buffer, const char *stripchars);
    class Tokenizer {
    public:
        static const std::string DEFAULT_DELIMITERS;
        Tokenizer(const std::string& str);
        Tokenizer(const std::string& str, const std::string& delimiters);

        bool nextToken();
        bool nextToken(const std::string& delimiters);
        const std::string getToken() const;
        void reset();
    protected:
        size_t m_Offset;
        const std::string m_String;
        std::string m_Token;
        std::string m_Delimiters;
    };
}

class CRPSGSolverHelper;

typedef void (*_PrintToR)(const char*, WHATPRINT_TYPE_R what);
typedef void (*_ProcessStopToR)(const char*, int);
typedef void (*_CycleFinishToR)(const char*, int);
typedef void (*_ProcessCancelToR)(void);
typedef int (*_RunRExternalFunction)(EXT_FUNCTION_DESCR_R*, int, const char*, double*, CRPSGSolverHelper*, double*);
typedef int (*_RunRGradientExternalFunction)(EXT_FUNCTION_DESCR_R*, int, const char*, double*, CRPSGSolverHelper*, double**);
typedef int (*_RunRExternalFunctionDir)(EXT_FUNCTION_DESCR_R*, const char*, int, double*, int, int*, CRPSGSolverHelper*, double*&, double*&);
typedef int (*_RunRGradientExternalFunctionDir)(EXT_FUNCTION_DESCR_R*, const char*, int, double*, int, int*, CRPSGSolverHelper*, double*&, double*&);

class  CRPSGSolverHelper : public IPSGSolverHelper
{
public:
	CRPSGSolverHelper(void);
	CRPSGSolverHelper(CPSGS_Problem* problem);
	virtual ~CRPSGSolverHelper(void);
private:
	CPSGS_Problem* m_problem;
	std::string m_errorstr;
#ifdef LINUX
	void *m_hRPSGFInstLibrary;
#else
	HINSTANCE  m_hRPSGFInstLibrary;
#endif
	_PrintToR m_PrintToRFun;
	_ProcessStopToR m_ProcessStopToRFun;
	_CycleFinishToR m_CycleFinishToRFun;
	_ProcessCancelToR m_ProcessCancelToRFun;
	_RunRExternalFunction m_RunRExternalFunction;
	_RunRGradientExternalFunction m_RunRGradientExternalFunction;
	_RunRExternalFunctionDir m_RunRExternalFunctionDir;
	_RunRGradientExternalFunctionDir m_RunRGradientExternalFunctionDir;
	int	m_nCancelStatus;
	HELPER_REGIME_TYPE_R m_whattodo;
	bool m_suppressMessages;
	bool m_suppressWarnings;
	bool m_suppressErrors;
	std::vector<EXT_FUNCTION_DESCR_R> m_extfunction_arr;
	bool m_key_block_newoutputproblem;
public:
	bool m_find_PrintToRFun(void);
	bool m_find_ProcessStopToRFun(void);
	bool m_find_CycleFinishToRFun(void);
	bool m_find_ProcessCancelToRFun(void);
	bool m_find_RunRExternalFunction(void);
	bool m_find_RunRGradientExternalFunction(void);
	bool m_find_RunRExternalFunctionDir(void);
	bool m_find_RunRGradientExternalFunctionDir(void);
	bool m_set_R_Functions(_PrintToR PrintToRFun,
			_ProcessStopToR ProcessStopToRFun, _CycleFinishToR CycleFinishToRFun,
			_ProcessCancelToR ProcessCancelToRFun, _RunRExternalFunction RunRExternalFunction,
			_RunRGradientExternalFunction RunRGradientExternalFunction,	_RunRExternalFunctionDir RunRExternalFunctionDir,
			_RunRGradientExternalFunctionDir RunRGradientExternalFunctionDir);
	void m_set_problem(CPSGS_Problem* this_problem) {m_problem = this_problem;}
	void m_set_block_newoutputproblem(bool key) {m_key_block_newoutputproblem = key;}
	void m_set_suppressMessages(bool suppressMessages) {m_suppressMessages = suppressMessages;}
	void m_set_m_suppressWarnings(bool suppressWarnings) {m_suppressWarnings = suppressWarnings;}
	void m_set_m_suppressErrors(bool suppressErrors) {m_suppressErrors = suppressErrors;}
	void m_set_whattodo(int whattodo);
	void m_set_whattodo(HELPER_REGIME_TYPE_R whattodo) {m_whattodo = whattodo;}
	HELPER_REGIME_TYPE_R m_get_whattodo(void) {return m_whattodo;}
	void clearerror(void) {m_errorstr.clear();}
	std::string m_getlasterror(void) {return m_errorstr;}
	void m_set_error(std::string error) {m_errorstr = error;}
	CPSGS_Problem* m_get_problem(void) {return m_problem;}
	CPSGS_OutputProblem* m_get_curr_output_problem(void);
	std::vector<EXT_FUNCTION_DESCR_R>* m_get_extfunction_arr(void) {return &m_extfunction_arr;}
	bool m_get_block_newoutputproblem(void) {return m_key_block_newoutputproblem;}
	virtual int Init();
	virtual void End();
	bool IsInit();
	bool IsProblemReadyToStart(void);
	bool IsProblemHasAllInputData(std::vector<std::string>& errors_arr);
	bool IsProblemHasAllInputData(std::vector<std::string>* point_arr, std::vector<std::string>* vector_arr, std::vector<std::string>* matrix_arr, std::vector<std::string>* pmatrix_arr, std::vector<std::string>& errors_arr);
	int CollectExternalFunctions(std::vector<std::string>& errors_arr);
	virtual int GetExternalFunctionInfo(const char* funcname, char*& pHeaderBuf, int *cols);
	virtual int RunGradientExternalFunction(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double** gradvalue);
	virtual int RunExternalFunction(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double* funvalue);
	virtual int SetTaskVerify(int verify) {return 0;}
	virtual int GetExternalFunctionInfoDir(const char* funcname, char*& pHeaderBuf, int* lenvars, int* lenscens);
	virtual int RunExternalFunctionDir(const char* funcname, char* pHeaderBuf, int lenvars, double* varvalues, int lenscens, int* numscens,
			 double*& probability, double*& funvalues);
	virtual int RunGradientExternalFunctionDir(const char* funcname, char* pHeaderBuf,
			int lenvars, double* varvalues, int lenscens, int* numscen, double*& probability, double*& gradvalue);
	virtual int GetProblemDescription(char*& pBuffer);
	virtual int GetPoint(const char* szPointName, char*& pBuffer);
	virtual int  GetMatrixInfo(const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols);
	virtual int  GetMatrixInfoSp(const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols, int* coeffs);
	virtual bool GetMatrixData(const char* lpszMatrixName, double*& pData,  int rows, int cols);
	virtual bool GetMatrixDataSp(const char* lpszMatrixName, double*& pData,  int*& prows, int*& pcols, int coeffs);
	virtual void ReleaseBuffer(void* pBuffer);
	virtual void ReleaseMatrix(double* pMatrix);
	virtual bool SaveObjective(char szObjectiveName[max_objectname_len], double value);
	virtual bool SaveVars(char szVarsName[][max_name_len], double values[], unsigned nCount);
	virtual bool SaveVector(const char* szVectorName, double varvalues[], unsigned nCount);
	virtual bool SaveMatrix(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow);
	virtual bool SavePoint(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount);
	virtual bool AddPoint(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount);
	virtual bool AddVector(const char* szVectorName, double varvalues[], unsigned nCount);
	virtual bool AddMatrix(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow);
	virtual bool SaveObjs(char szObjName[][max_objectname_len], double val[], char szConstrName[][max_name_len], char type[], unsigned nCount);
	virtual bool SaveConstraints(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount );
	virtual bool SaveConstraintsSlack(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount );
	virtual bool SaveStatus(const char* szStatus);
	virtual void OnMessage(int nMsgCode, char* szMessage);
	virtual void OnError(int nErrCode, const char* lpszObject, const char* lpszDescription);
	virtual void OnWarning(int nWarnCode, const char* lpszObject, const char* lpszDescription);
	virtual int  OnCancel(void);
	virtual int OnFinish(void);
	virtual int OnCycleFinish(void);
	virtual void GetRootPath(char* pBuffer);
	virtual int GetLogParam( char* pBuffer ) {return -1;};
	virtual int SaveProblemName( char* pBuffer );
	virtual int SaveProblemStatement(char* problemstatement);
	virtual int SaveSolution(char* problemsolution);
	virtual void OnProcessStop(int nErrCode);
	virtual int SetMultyProblem(int ismulty);
	static int ImportProblemFromTextFiles(std::string pathtofile, CRPSGSolverHelper* thisImpl);
	static int ExportProblemToTextFiles(std::string pathtofile, CRPSGSolverHelper* thisImpl);
	static int GetPSGDataObjectsFromStatement(std::string statement, CRPSGSolverHelper* thisImpl, 
		std::vector<std::string>& point_arr, std::vector<std::string>& vector_arr, std::vector<std::string>& matrix_arr, std::vector<std::string>& pmatrix_arr, std::vector<std::string>& added_objects_arr, 
			std::string& errorstr);
	static int GetPSGDataObjectsFromStatementDirect(std::string statement, CRPSGSolverHelper* thisImpl, std::string& firstterm, std::string& secondterm,
		std::vector<std::string>& point_arr, std::vector<std::string>& vector_arr, std::vector<std::string>& matrix_arr, std::vector<std::string>& pmatrix_arr, 
			std::string& errorstr);
	static std::string remove_comments_fromstring(std::string thisstr);
	static int run_rpsg_solver(CRPSGSolverHelper* thisImpl);
	static int run_rpsg_verify(CRPSGSolverHelper* thisImpl);
	static int get_Function_Value_1(CRPSGSolverHelper* thisImpl, const char* pBuffer, const char* pPointName, double* fvalue);
	static int get_Function_Gradient_1(CRPSGSolverHelper* thisImpl, const char* pBuffer, const char* pPointName, CPSGS_Data_Object** pp_this_point, double* fValue);
	static int get_Function_Increment_1(CRPSGSolverHelper* thisImpl, const char* pBuffer, const char* pPointName, CPSGS_Data_Object** pp_this_point, double* fValue);
};
