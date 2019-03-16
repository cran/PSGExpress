#include "RPSGSolverHelper.h"
#include "../../PSGSolver/src/PSGSolver.h"
#include <sstream>
#include <iomanip>
#include <assert.h>
#include <algorithm>
#ifdef WIN32
#include <windows.h>
#elif LINUX
#include <link.h>
#include <termios.h>
#include <sys/ioctl.h>
#else
#error Platform not supported
#endif
#include <stdio.h>
#include <string.h>
#include <string>
#include <unistd.h>
#include <ctype.h>
#include <inttypes.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>

using namespace std;

extern "C"
{
	int  vk_getfunctionvalue(const char* pBuffer, const char* pPointName, double* fValue, void* pUserData);
	int  vk_getfunctiongradient(const char* pBuffer, const char* pPointName, double* fValue, void* pUserData);
	int  vk_getfunctionincrement(const char* pBuffer, const char* pPointName, double* fValue, void* pUserData);
	void  getoneproblemfromcycle_psg(char* pChar, int ibuff, int numbcall, char* pOut, int ibout, int* irez, void* pUserData);
}

int read_matrix_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, int64_t& n_cols, std::string& errorstr);
int read_pmatrix_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, int64_t& n_cols, int64_t& n_nz, std::string& errorstr);
int read_vector_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, std::string& errorstr);
int read_point_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, std::string& errorstr);

namespace strutil_Helper {
    using namespace std;
#ifdef LINUX
char * strtok_uni(char *str, const char *delim, char **saveptr)
	{
    	return strtok_r(str, delim, saveptr);
	}
#else
char *strtok_uni(char *str, const char *delim, char **saveptr)
	{
    	return strtok(str, delim);
	}
#endif
#ifdef LINUX
    int __attribute__ ((visibility("hidden"))) Rmytolower(int c) {return tolower(c);}
#else
    int Rmytolower(int c) {return tolower(c);}
#endif
#ifdef LINUX
    int __attribute__ ((visibility("hidden"))) Rmytoupper(int c) {return toupper(c);}
#else
    int Rmytoupper(int c) {return toupper(c);}
#endif

#ifdef LINUX
    string __attribute__ ((visibility("hidden"))) trim(const string& str)
#else
    string trim(const string& str)
#endif
	{
		if(str.length() == 0)
			return str;
		string::size_type beg = str.find_first_not_of(" \a\b\f\n\r\t\v");
		string::size_type end = str.find_last_not_of(" \a\b\f\n\r\t\v");
		if(beg == string::npos)
			return "";
		return string(str, beg, end - beg + 1);
    }

#ifdef LINUX
    string __attribute__ ((visibility("hidden"))) RtoLower(const string& str)
#else
	string RtoLower(const string& str)
#endif
	{
        string t = str;
        transform(t.begin(), t.end(), t.begin(), Rmytolower);
        return t;
    }

#ifdef LINUX
    string __attribute__ ((visibility("hidden"))) RtoUpper(const string& str)
#else
    string RtoUpper(const string& str)
#endif
	{
        string t = str;
        transform(t.begin(), t.end(), t.begin(), Rmytoupper);
        return t;
    }
#ifdef LINUX
    vector<string> __attribute__ ((visibility("hidden"))) split(const string& str, const string& delimiters, bool bKeepBlanks)
#else
    vector<string> split(const string& str, const string& delimiters, bool bKeepBlanks)
#endif
	{
        vector<string> ss;

        Tokenizer tokenizer(str, delimiters);
        while (tokenizer.nextToken())
		{
			if(!bKeepBlanks && tokenizer.getToken().empty())
				continue;
            ss.push_back(tokenizer.getToken());
        }

        return ss;
    }

#ifdef LINUX
    char __attribute__ ((visibility("hidden"))) *chartrim(char *buffer, const char *stripchars)
#else
    char *chartrim(char *buffer, const char *stripchars)
#endif
    {
        int i = 0;
        int flag;
        char *start = buffer;
        do {
            flag = 0;
            for (i = 0; i < (int)strlen(stripchars); i++) {
                if (*start == stripchars[i]) {
                    start++;
                    flag = 1;
                    break;
                }
            }
        } while (flag);
        char *end = start + strlen(start) - 1;
        do {
            flag = 0;
            for (i = 0; i < (int)strlen(stripchars); i++) {
                if (*end == stripchars[i]) {
                    *end = '\0';
                    --end;
                    flag = 1;
                    break;
                }
            }
        } while (flag);
        return start;
    }
    const string Tokenizer::DEFAULT_DELIMITERS(" \t\n\r");
    Tokenizer::Tokenizer(const std::string& str)
        : m_Offset(0), m_String(str), m_Delimiters(DEFAULT_DELIMITERS) {}

    Tokenizer::Tokenizer(const std::string& str, const std::string& delimiters)
        : m_Offset(0), m_String(str), m_Delimiters(delimiters) {}

    bool Tokenizer::nextToken() {
        return nextToken(m_Delimiters);
    }
    bool Tokenizer::nextToken(const std::string& delimiters) {
        size_t i = m_String.find_first_not_of(delimiters, m_Offset);
        if (i == string::npos) {
            m_Offset = m_String.length();
            return false;
        }
        size_t j = m_String.find_first_of(delimiters, i);
        if (j == string::npos) {
            m_Token = m_String.substr(i);
            m_Offset = m_String.length();
            return true;
        }
        m_Token = m_String.substr(i, j - i);
        m_Offset = j;
        return true;
    }
    const string Tokenizer::getToken() const {
        return m_Token;
    }
    void Tokenizer::reset() {
        m_Offset = 0;
    }
}

CRPSGSolverHelper::CRPSGSolverHelper(void):
	  m_problem(NULL)
	, m_hRPSGFInstLibrary(NULL)
	, m_PrintToRFun(NULL)
	, m_ProcessStopToRFun(NULL)
	, m_CycleFinishToRFun(NULL)
	, m_ProcessCancelToRFun(NULL)
	, m_RunRExternalFunction(NULL)
	, m_RunRGradientExternalFunction(NULL)
	, m_RunRExternalFunctionDir(NULL)
	, m_RunRGradientExternalFunctionDir(NULL)
    , m_whattodo(HELPER_REGIME_SOLVE_R)
	, m_suppressMessages(false)
	, m_suppressWarnings(false)
	, m_suppressErrors(false)
	, m_key_block_newoutputproblem(false)
{}

CRPSGSolverHelper::CRPSGSolverHelper(CPSGS_Problem* problem) : 
  	  m_problem(problem)
	, m_hRPSGFInstLibrary(NULL)
	, m_PrintToRFun(NULL)
	, m_ProcessStopToRFun(NULL)
	, m_CycleFinishToRFun(NULL)
	, m_ProcessCancelToRFun(NULL)
	, m_RunRExternalFunction(NULL)
	, m_RunRGradientExternalFunction(NULL)
	, m_RunRExternalFunctionDir(NULL)
	, m_RunRGradientExternalFunctionDir(NULL)
    , m_whattodo(HELPER_REGIME_SOLVE_R)
	, m_suppressMessages(false)
	, m_key_block_newoutputproblem(false)
{}

CRPSGSolverHelper::~CRPSGSolverHelper(void)
{
	if (m_problem) {
		delete m_problem;
	}
	if (m_hRPSGFInstLibrary) {
		m_hRPSGFInstLibrary = NULL;
	}
}

bool CRPSGSolverHelper::m_find_PrintToRFun(void)
{
	if (!m_hRPSGFInstLibrary) return false;
#ifdef WIN32
	m_PrintToRFun = reinterpret_cast< _PrintToR >( GetProcAddress( m_hRPSGFInstLibrary, "PrintToR" ) );
#elif LINUX
	m_PrintToRFun = reinterpret_cast< _PrintToR >( dlsym( m_hRPSGFInstLibrary, "PrintToR" ) );
#else
#error Platform not supported
#endif
	return m_PrintToRFun!=NULL;
}

bool CRPSGSolverHelper::m_find_ProcessStopToRFun(void)
{
	if (!m_hRPSGFInstLibrary) return false;
#ifdef WIN32
	m_ProcessStopToRFun = reinterpret_cast< _ProcessStopToR >( GetProcAddress( m_hRPSGFInstLibrary, "ProcessStopToR" ) );
#elif LINUX
	m_ProcessStopToRFun = reinterpret_cast< _ProcessStopToR >( dlsym( m_hRPSGFInstLibrary, "ProcessStopToR" ) );
#else
#error Platform not supported
#endif
	return m_ProcessStopToRFun!=NULL;
}

bool CRPSGSolverHelper::m_find_CycleFinishToRFun(void)
{
	if (!m_hRPSGFInstLibrary) return false;
#ifdef WIN32
	m_CycleFinishToRFun = reinterpret_cast< _CycleFinishToR >( GetProcAddress( m_hRPSGFInstLibrary, "CycleFinishToR" ) );
#elif LINUX
	m_CycleFinishToRFun = reinterpret_cast< _CycleFinishToR >( dlsym( m_hRPSGFInstLibrary, "CycleFinishToR" ) );
#else
#error Platform not supported
#endif
	return m_CycleFinishToRFun!=NULL;
}

bool CRPSGSolverHelper::m_find_ProcessCancelToRFun(void)
{
	if (!m_hRPSGFInstLibrary) {
		return false;
	}
#ifdef WIN32
	m_ProcessCancelToRFun = reinterpret_cast< _ProcessCancelToR >( GetProcAddress( m_hRPSGFInstLibrary, "ProcessCancelToR" ) );
#elif LINUX
	m_ProcessCancelToRFun = reinterpret_cast< _ProcessCancelToR >( dlsym( m_hRPSGFInstLibrary, "ProcessCancelToR" ) );
#else
#error Platform not supported
#endif
	return m_ProcessCancelToRFun!=NULL;
}

bool CRPSGSolverHelper::m_find_RunRExternalFunction(void)
{
	if (!m_hRPSGFInstLibrary) {
		return false;
	}
#ifdef WIN32
	m_RunRExternalFunction = reinterpret_cast< _RunRExternalFunction >( GetProcAddress( m_hRPSGFInstLibrary, "RunRExternalFunction" ) );
#elif LINUX
	m_RunRExternalFunction = reinterpret_cast< _RunRExternalFunction >( dlsym( m_hRPSGFInstLibrary, "RunRExternalFunction" ) );
#else
#error Platform not supported
#endif
	return m_RunRExternalFunction!=NULL;
}

bool CRPSGSolverHelper::m_find_RunRGradientExternalFunction(void)
{
	if (!m_hRPSGFInstLibrary) {
		return false;
	}
#ifdef WIN32
	m_RunRGradientExternalFunction = reinterpret_cast< _RunRGradientExternalFunction >( GetProcAddress( m_hRPSGFInstLibrary, "RunRGradientExternalFunction" ) );
#elif LINUX
	m_RunRGradientExternalFunction = reinterpret_cast< _RunRGradientExternalFunction >( dlsym( m_hRPSGFInstLibrary, "RunRGradientExternalFunction" ) );
#else
#error Platform not supported
#endif
	return m_RunRGradientExternalFunction!=NULL;
}

bool CRPSGSolverHelper::m_find_RunRExternalFunctionDir(void)
{
	if (!m_hRPSGFInstLibrary) {
		return false;
	}
#ifdef WIN32
	m_RunRExternalFunctionDir = reinterpret_cast< _RunRExternalFunctionDir >( GetProcAddress( m_hRPSGFInstLibrary, "RunRExternalFunctionDir" ) );
#elif LINUX
	m_RunRExternalFunctionDir = reinterpret_cast< _RunRExternalFunctionDir >( dlsym( m_hRPSGFInstLibrary, "RunRExternalFunctionDir" ) );
#else
#error Platform not supported
#endif
	return m_RunRExternalFunctionDir!=NULL;
}

bool CRPSGSolverHelper::m_find_RunRGradientExternalFunctionDir(void)
{
	if (!m_hRPSGFInstLibrary) {
		return false;
	}
#ifdef WIN32
	m_RunRGradientExternalFunctionDir = reinterpret_cast< _RunRGradientExternalFunctionDir >( GetProcAddress( m_hRPSGFInstLibrary, "RunRGradientExternalFunctionDir" ) );
#elif LINUX
	m_RunRGradientExternalFunctionDir = reinterpret_cast< _RunRGradientExternalFunctionDir >( dlsym( m_hRPSGFInstLibrary, "RunRGradientExternalFunctionDir" ) );
#else
#error Platform not supported
#endif
	return m_RunRGradientExternalFunctionDir!=NULL;
}

bool CRPSGSolverHelper::m_set_R_Functions(
	 	_PrintToR PrintToRFun,
		_ProcessStopToR ProcessStopToRFun,
		_CycleFinishToR CycleFinishToRFun,
		_ProcessCancelToR ProcessCancelToRFun,
		_RunRExternalFunction RunRExternalFunction,
		_RunRGradientExternalFunction RunRGradientExternalFunction,
		_RunRExternalFunctionDir RunRExternalFunctionDir,
		_RunRGradientExternalFunctionDir RunRGradientExternalFunctionDir
		)
{
 	m_PrintToRFun = PrintToRFun;
	m_ProcessStopToRFun = ProcessStopToRFun;
	m_CycleFinishToRFun = CycleFinishToRFun;
	m_ProcessCancelToRFun = ProcessCancelToRFun;
	m_RunRExternalFunction = RunRExternalFunction;
	m_RunRGradientExternalFunction = RunRGradientExternalFunction;
	m_RunRExternalFunctionDir = RunRExternalFunctionDir;
	m_RunRGradientExternalFunctionDir = RunRGradientExternalFunctionDir;
	return true;
}

void CRPSGSolverHelper::m_set_whattodo(int whattodo)
{
	switch (whattodo) { //
	case 2:
		m_whattodo = HELPER_REGIME_SOLVE_R; break;
	case 3:
		m_whattodo = HELPER_REGIME_VERIFY_R; break;
	case 4:
		m_whattodo = HELPER_REGIME_INTERRUPT_R; break;
	default:
		m_whattodo = HELPER_REGIME_UNKNOWN_R; break;
	}
	return;
}

int CRPSGSolverHelper::Init()
{
	int nret = 0;
	if( (nret = IPSGSolverHelper::Init()) == -1)
		return -1;
	return nret;
}

void CRPSGSolverHelper::End()
{
	IPSGSolverHelper::End();
}

bool CRPSGSolverHelper::IsInit()
{
	return IPSGSolverHelper::IsInit();
}

void CRPSGSolverHelper::OnMessage( int nMsgCode, char* szMessage )
{
	char* pch;
	char str[] = "(sec)";
	char str1[] = "Data";
	char str2[] = "Preprocessing";
	char str3[] = "Solving";
	char seps[]   = " \t\n";
	char *token;
	pch = strstr(szMessage, str);
	if (pch) {
		token = strtok(pch, seps);
		token = strtok(NULL, seps);
		if (strstr(szMessage, str1)) {
			if (m_problem!=NULL) {
				CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
				if (this_curr_problem!=NULL) {
					this_curr_problem->m_set_loading_time(atof(token));
				}
			}
			return;
		}
		else if (strstr(szMessage, str2)) {
			if (m_problem!=NULL) {
				CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
				if (this_curr_problem!=NULL) {
					this_curr_problem->m_set_preprocessing_time(atof(token));
				}
			}
			return;
		}
		else if (strstr(szMessage, str3)) {
			if (m_problem!=NULL) {
				CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
				if (this_curr_problem!=NULL) {
					this_curr_problem->m_set_solving_time(atof(token));
				}
			}
			return;
		}
	}
	if (m_problem!=NULL) {
		CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
		if (this_curr_problem!=NULL) {
			this_curr_problem->add_to_log(string(szMessage));
		}
	}
	if (m_PrintToRFun) {
		if (!m_suppressMessages) m_PrintToRFun(szMessage, WHATPRINT_MESSAGE_R);
	}
	return;
}

void CRPSGSolverHelper::OnError( int nErrCode, const char* lpszObject, const char* lpszDescription )
{
	char msg[1024];
	snprintf(msg, 1024, "Error: %i Object: %s\tMessage: %s", nErrCode, lpszObject, lpszDescription);
	if (m_problem!=NULL) {
		CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
		if (this_curr_problem!=NULL) {
			this_curr_problem->add_to_log(string(msg));
			this_curr_problem->add_to_errors(string(msg));
		}
	}
	if (m_PrintToRFun) {
		if (!m_suppressErrors) m_PrintToRFun(msg, WHATPRINT_ERROR_R);
	}
}

void CRPSGSolverHelper::OnWarning( int nWarnCode, const char* lpszObject, const char* lpszDescription )
{
	char msg[1024];
	snprintf(msg, 1024, "Warning: %i Object: %s\tMessage: %s", nWarnCode, lpszObject, lpszDescription);
	if (m_problem!=NULL) {
		CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
		if (this_curr_problem!=NULL) {
			this_curr_problem->add_to_log(string(msg));
			this_curr_problem->add_to_warnings(string(msg));
		}
	}
	if (m_PrintToRFun) {
		if (!m_suppressWarnings) m_PrintToRFun(msg, WHATPRINT_WARNING_R);
	}
}

int CRPSGSolverHelper::RunExternalFunctionDir(const char* funcname, char* pHeaderBuf, int lenvars, double* varvalues, int lenscens, int* numscens,
		 double*& probability, double*& funvalues)
{
	m_errorstr.clear();
	if (m_extfunction_arr.empty()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found (collection is empty)", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	vector<EXT_FUNCTION_DESCR_R>::iterator it_descr = find(m_extfunction_arr.begin(), m_extfunction_arr.end(), string(funcname));
	if (it_descr==m_extfunction_arr.end()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->fntype != FUNTYPE_DIR) {
		char buffererr[256];
		sprintf(buffererr, "%s has wrong type", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->str_fn_vars.empty()) {
		char buffererr[256];
		sprintf(buffererr, "Variables for %s not defined", it_descr->fnname.c_str());
		m_errorstr = string(buffererr);
		return -1;
	}
	it_descr->fnruncount++;
	return m_RunRExternalFunctionDir(&(*it_descr), (const char*)pHeaderBuf, lenvars, varvalues, lenscens, numscens,
			 this, probability, funvalues);
}

int CRPSGSolverHelper::RunExternalFunction(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double* funvalue)
{
	// 1 - OK 0 - fail
	m_errorstr.clear();
	if (m_extfunction_arr.empty()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found (collection is empty)", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	vector<EXT_FUNCTION_DESCR_R>::iterator it_descr = find(m_extfunction_arr.begin(), m_extfunction_arr.end(), string(funcname));
	if (it_descr==m_extfunction_arr.end()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->fntype != FUNTYPE_REG) {
		char buffererr[256];
		sprintf(buffererr, "%s has wrong type", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->str_fn_vars.empty()) {
		char buffererr[256];
		sprintf(buffererr, "Variables for %s not defined", it_descr->fnname.c_str());
		m_errorstr = string(buffererr);
		return -1;
	}
	it_descr->fnruncount++;
	return m_RunRExternalFunction(&(*it_descr), numvar, (const char*)pHeaderBuf, varvalues, this, funvalue);
}

int CRPSGSolverHelper::RunGradientExternalFunctionDir(const char* funcname, char* pHeaderBuf,
		int lenvars, double* varvalues, int lenscens, int* numscen, double*& probability, double*& gradvalue)
{
	m_errorstr.clear();
	if (m_extfunction_arr.empty()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found (collection is empty)", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	vector<EXT_FUNCTION_DESCR_R>::iterator it_descr = find(m_extfunction_arr.begin(), m_extfunction_arr.end(), string(funcname));
	if (it_descr==m_extfunction_arr.end()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->fntype != FUNTYPE_DIR) {
		char buffererr[256];
		sprintf(buffererr, "%s has wrong type", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->str_fn_vars.empty()) {
		char buffererr[256];
		sprintf(buffererr, "Variables for %s not defined", it_descr->fnname.c_str());
		m_errorstr = string(buffererr);
		return -1;
	}
	it_descr->grruncount++;
	return m_RunRGradientExternalFunctionDir(&(*it_descr), (const char*)pHeaderBuf,
			lenvars, varvalues, lenscens, numscen, this, probability, gradvalue);
}

int CRPSGSolverHelper::RunGradientExternalFunction(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double** gradvalue)
{
	m_errorstr.clear();
	if (m_extfunction_arr.empty()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found (collection is empty)", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	vector<EXT_FUNCTION_DESCR_R>::iterator it_descr = find(m_extfunction_arr.begin(), m_extfunction_arr.end(), string(funcname));
	if (it_descr==m_extfunction_arr.end()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->fntype != FUNTYPE_REG) {
		char buffererr[256];
		sprintf(buffererr, "%s has wrong type", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->str_fn_vars.empty()) {
		char buffererr[256];
		sprintf(buffererr, "Variables for %s not defined", it_descr->fnname.c_str());
		m_errorstr = string(buffererr);
		return -1;
	}
	it_descr->grruncount++;
	return m_RunRGradientExternalFunction(&(*it_descr), numvar, (const char*)pHeaderBuf, varvalues, this, gradvalue);
}

int CRPSGSolverHelper::GetExternalFunctionInfoDir(const char* funcname, char*& pHeaderBuf, int* lenvars, int* lenscens)
{
	m_errorstr.clear();
	if (m_extfunction_arr.empty()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found (collection is empty)", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	vector<EXT_FUNCTION_DESCR_R>::iterator it_descr = find(m_extfunction_arr.begin(), m_extfunction_arr.end(), string(funcname));
	if (it_descr==m_extfunction_arr.end()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->fntype != FUNTYPE_DIR) {
		char buffererr[256];
		sprintf(buffererr, "%s has wrong type", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->str_fn_vars.empty()) {
		char buffererr[256];
		sprintf(buffererr, "Variables for %s not defined", it_descr->fnname.c_str());
		m_errorstr = string(buffererr);
		return -1;
	}
	*lenvars = (int)it_descr->str_fn_vars.size();
	string strout = it_descr->str_fn_vars[0];
	for (size_t i=1; i<it_descr->str_fn_vars.size(); i++) {
		strout += string("\t") + it_descr->str_fn_vars[i];
	}
	size_t lenbuffer = strout.size()+1;
	if((pHeaderBuf = (char*)malloc(lenbuffer * sizeof(char))) == NULL) {
		char buffererr[256];
		sprintf(buffererr, "Cannot allocate memory for %s", it_descr->fnname.c_str());
		m_errorstr = string(buffererr);
		return -1;
	}
	memset(pHeaderBuf, 0, lenbuffer);
	strcpy(pHeaderBuf, strout.c_str());
	*lenscens = it_descr->numscen;
	return (int)lenbuffer;
}

int CRPSGSolverHelper::GetExternalFunctionInfo(const char* funcname, char*& pHeaderBuf, int *cols)
{
	m_errorstr.clear();
	if (m_extfunction_arr.empty()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found (collection is empty)", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	vector<EXT_FUNCTION_DESCR_R>::iterator it_descr = find(m_extfunction_arr.begin(), m_extfunction_arr.end(), string(funcname));
	if (it_descr==m_extfunction_arr.end()) {
		char buffererr[256];
		sprintf(buffererr, "%s not found", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->fntype != FUNTYPE_REG) {
		char buffererr[256];
		sprintf(buffererr, "%s has wrong type", funcname);
		m_errorstr = string(buffererr);
		return -1;
	}
	if (it_descr->str_fn_vars.empty()) {
		char buffererr[256];
		sprintf(buffererr, "Variables for %s not defined", it_descr->fnname.c_str());
		m_errorstr = string(buffererr);
		return -1;
	}
	*cols = (int)it_descr->str_fn_vars.size();
	string strout = it_descr->str_fn_vars[0];
	for (size_t i=1; i<it_descr->str_fn_vars.size(); i++) {
		strout += string("\t") + it_descr->str_fn_vars[i];
	}
	size_t lenbuffer = strout.size()+1;
	if((pHeaderBuf = (char*)malloc(lenbuffer * sizeof(char))) == NULL) {
		char buffererr[256];
		sprintf(buffererr, "Cannot allocate memory for %s", it_descr->fnname.c_str());
		m_errorstr = string(buffererr);
		return -1;
	}
	memset(pHeaderBuf, 0, lenbuffer);
	strcpy(pHeaderBuf, strout.c_str());
	return (int)lenbuffer;
}

bool CRPSGSolverHelper::IsProblemReadyToStart(void)
{
	if (m_problem==NULL) return false;
	if (m_problem->m_is_problem_empty()) return false;
	return true;
}

bool CRPSGSolverHelper::IsProblemHasAllInputData(vector<string>* point_arr, vector<string>* vector_arr, vector<string>* matrix_arr, vector<string>* pmatrix_arr, vector<string>& errors_arr)
{
	errors_arr.clear();
	string errorstr;
	string statement = m_problem->m_get_statement();
	bool res = true;
	if (matrix_arr->size()>0) {
		for(vector<string>::iterator it_name=matrix_arr->begin(); it_name!=matrix_arr->end(); it_name++) {
			if (!m_problem->m_isin_input_collection(*it_name)) {
				errors_arr.push_back(*it_name);
				res = false;
			}
		}
	}
	if (pmatrix_arr->size()>0) {
		for(vector<string>::iterator it_name=pmatrix_arr->begin(); it_name!=pmatrix_arr->end(); it_name++) {
			if (!m_problem->m_isin_input_collection(*it_name)) {
				errors_arr.push_back(*it_name);
				res = false;
			}
		}
	}
	if (point_arr->size()>0) {
		for(vector<string>::iterator it_name=point_arr->begin(); it_name!=point_arr->end(); it_name++) {
			if (!m_problem->m_isin_input_collection(*it_name)) {
				errors_arr.push_back(*it_name);
				res = false;
			}
		}
	}
	if (vector_arr->size()>0) {
		for(vector<string>::iterator it_name=vector_arr->begin(); it_name!=vector_arr->end(); it_name++) {
			if (!m_problem->m_isin_input_collection(*it_name)) {
				errors_arr.push_back(*it_name);
				res = false;
			}
		}
	}
	return res;
}

bool CRPSGSolverHelper::IsProblemHasAllInputData(vector<string>& errors_arr)
{
	errors_arr.clear();
	string errorstr;
	string statement = m_problem->m_get_statement();
	vector<string> point_arr, vector_arr, matrix_arr, pmatrix_arr, added_objects_arr;
	if (GetPSGDataObjectsFromStatement(statement, this, point_arr, vector_arr, matrix_arr, pmatrix_arr, added_objects_arr, errorstr)<0) {
		errors_arr.push_back(errorstr);
		return false;
	}
	return this->IsProblemHasAllInputData(&point_arr, &vector_arr, &matrix_arr, &pmatrix_arr, errors_arr);
}

size_t findExtFunDescr(string statement, EXT_FUNTYPE& thistype, size_t from)
{
	size_t mpos_usual = statement.find("fnext_", from);
	size_t mpos_dir = statement.find("fnextdir_", from);
	size_t mpos=string::npos;
	thistype = FUNTYPE_UNKNOWN;
	if (mpos_usual!=string::npos) {
		thistype = FUNTYPE_REG;	mpos = mpos_usual;
	}
	if (mpos_dir!=string::npos) {
		if (mpos_usual!=string::npos) {
			if (mpos_usual>mpos_dir) {
				thistype = FUNTYPE_DIR;	mpos = mpos_dir;
			}
		}
		else {
			thistype = FUNTYPE_DIR;	mpos = mpos_dir;
		}
	}
	return mpos;
}


int CRPSGSolverHelper::CollectExternalFunctions(std::vector<std::string>& errors_arr)
{
	errors_arr.clear();
	if (m_problem==NULL) { 
		errors_arr.push_back("Problem not defined");
		return -1;
	}
	m_extfunction_arr.clear();
	string problem_statement = m_problem->m_get_statement();
	if (problem_statement.empty()) return 0;
	int res=1;
	EXT_FUNTYPE thistype;
	size_t mpos = findExtFunDescr(problem_statement, thistype, 0);
	if (mpos==string::npos) {
		return 0;
	}
	while (mpos != string::npos) {
		size_t pos_b1 = mpos;
		size_t pos_b2 = problem_statement.find("(", pos_b1+1);
		size_t pos_e;
		if (pos_b2 != string::npos) {
			pos_e = problem_statement.find(")", pos_b2+1);
			if (pos_e != string::npos) { // OK, found
				string thisdescr = problem_statement.substr(pos_b1, pos_e-pos_b1+1);
				string thisfname = problem_statement.substr(pos_b1, pos_b2-pos_b1);
				thisdescr.erase(remove(thisdescr.begin(), thisdescr.end(), ' '), thisdescr.end());
				thisfname.erase(remove(thisfname.begin(), thisfname.end(), ' '), thisfname.end());
				string thisargs = problem_statement.substr(pos_b2+1, pos_e-pos_b2-1);
				vector<string> body_str_arr = strutil_Helper::split(thisargs, string(" ,\t\r\n[]"), false);
				if (body_str_arr.empty()) {
					char buffererr[1024];
					sprintf(buffererr, "%s should has from 1 to 4 arguments", thisdescr.c_str());
					errors_arr.push_back(buffererr);
					res = -1;
					mpos = findExtFunDescr(problem_statement, thistype, mpos+1);
					continue;
				}
				string thisgrname = string("gr")+string(thisfname.c_str()+2);
				if (find(m_extfunction_arr.begin(), m_extfunction_arr.end(), thisdescr) == m_extfunction_arr.end()) {
					m_extfunction_arr.push_back(EXT_FUNCTION_DESCR_R(thisdescr, thisfname, thisgrname, thistype, body_str_arr));
				}
			}
		}
		mpos = findExtFunDescr(problem_statement, thistype, mpos+1);
		continue;
	}
	if (res>=0) res = (int)m_extfunction_arr.size();
	return res;
}

CPSGS_OutputProblem* CRPSGSolverHelper::m_get_curr_output_problem(void)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Getting problem output object", m_errorstr.c_str());
		return NULL;
	}
	return m_problem->m_get_curr_output_problem();
}


int CRPSGSolverHelper::GetProblemDescription(char*& pBuffer)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Getting problem description", m_errorstr.c_str());
		return -1;
	}
	string this_description;
	string loc;
	switch (m_whattodo) {
		case HELPER_REGIME_UNKNOWN_R:
			this_description.clear();
			char buffererr[1024];
			loc = m_problem->m_get_name();
			sprintf(buffererr, "%s: Unknown Regime", loc.c_str());
			m_errorstr = string(buffererr);
			OnError(0xFFF1, "Getting problem description", m_errorstr.c_str());
			return -1;
		break;
		case HELPER_REGIME_SOLVE_R:
			this_description = m_problem->m_get_statement();
		break;
		case HELPER_REGIME_VERIFY_R:
			this_description = m_problem->m_get_statement();
		break;
		case HELPER_REGIME_INTERRUPT_R:
    	break;
		case HELPER_REGIME_SOLVED_R:
    	break;
	}
	if (this_description.empty()) {
		char buffererr[1024];
		sprintf(buffererr, "Description for %s is empty", m_problem->m_get_name().c_str());
		m_errorstr = string(buffererr);
		OnError(0xFFF1, "Getting problem description", m_errorstr.c_str());
		return -1;
	}
	size_t szOfBuff = this_description.size();
	if((pBuffer = (char*)malloc((szOfBuff + 1) * sizeof(char))) == NULL) {
		OnError(0xFFF1, "Getting problem description", "There is not enough memory available");
		return -1;
	}
	memset(pBuffer, 0, szOfBuff + 1);
	this_description.copy(pBuffer, szOfBuff);
	return static_cast<int>(szOfBuff) + 1;
}

int CRPSGSolverHelper::GetPoint(const char* szPointName, char*& pBuffer)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Getting Point", m_errorstr.c_str());
		return -1;
	}
	CPSGS_Data_Object* this_point = m_problem->m_getfrom_input_collection(string(szPointName), PSGS_POINT);
	if (!this_point) {
		string errMsg("Point ");
		errMsg += string(szPointName);
		errMsg += " not found in internal collection";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Point", errMsg.c_str());
		return -1;
	}
	vector<string>* this_header_arr = this_point->m_get_headrarray();
	double* this_data_arr = this_point->m_get_data();
	if (this_header_arr->empty() || (this_data_arr==NULL)) {
		string errMsg("Point ");
		errMsg += string(szPointName);
		errMsg += " has corrupted data";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Point", errMsg.c_str());
		return -1;
	}
	if ((this_point->m_get_size(1) != 1) || (this_point->m_get_size(0) != (int64_t)this_header_arr->size())) {
		string errMsg("Point ");
		errMsg += string(szPointName);
		errMsg += string(" has inconsistent data format");
		m_errorstr = errMsg;
		OnError(0xFFF1, "Point", errMsg.c_str());
		return -1;
	}
	int idxOfData = 0;
	stringstream tmpBuff;
	tmpBuff << "component_name\tvalue\n";
	for (vector<string>::iterator it_head=this_header_arr->begin(); it_head!=this_header_arr->end(); it_head++) {
		tmpBuff << (*it_head) << "\t" << setprecision(12) << this_data_arr[idxOfData++] << endl;
	}
	size_t szBuff = tmpBuff.str().length();
	if((pBuffer = (char*)malloc(szBuff + 1)) == NULL) {
		OnError(0xFFF1, "Point", "There is not enough memory available");
		return -1;
	}
	memset(pBuffer, 0, szBuff + 1);
	tmpBuff.str().copy(pBuffer, szBuff);
	return static_cast<int>(szBuff) + 1;
}

int CRPSGSolverHelper::GetMatrixInfo( const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols )
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Getting Matrix Info", m_errorstr.c_str());
		return -1;
	}
	PSGS_DATA_OBJECT_TYPE thistype=PSGS_DATAOBJUNKNOWN;
	string locmatrixname(lpszMatrixName);
	string errorobjname;
	locmatrixname = strutil_Helper::RtoLower(locmatrixname);
	if (locmatrixname.find(string("matrix_"))==0) {
		thistype = PSGS_MATRIX;
		errorobjname = "Matrix";
	}
	else if (locmatrixname.find(string("vector_"))==0) {
		thistype = PSGS_VECTOR;
		errorobjname = string("Vector");
	}
	else {
		string errMsg = string(lpszMatrixName);
		errMsg += " is not Matrix or Vector";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix//Vector", errMsg.c_str());
		return false;
	}
	CPSGS_Data_Object* this_matrix = m_problem->m_getfrom_input_collection(string(lpszMatrixName), thistype);
	if (!this_matrix) {
		string errMsg = errorobjname+string(" ");
		errMsg += string(lpszMatrixName);
		errMsg += " not found in internal collection";
		m_errorstr = errMsg;
		OnError(0xFFF1, errorobjname.c_str(), errMsg.c_str());
		return -1;
	}
	if (this_matrix->m_get_ndims() != 2) {
		string errMsg = errorobjname+string(" ");
		errMsg += string(lpszMatrixName);
		errMsg += string(" has inconsistent data format");
		m_errorstr = errMsg;
		OnError(0xFFF1, errorobjname.c_str(), errMsg.c_str());
		return -1;
	}
	*rows = this_matrix->m_get_size(0);
	*cols = this_matrix->m_get_size(1);

	int lmax = MAXVARNUMBER; // !!!Express!!!
	if (*cols>lmax) {
		*rows = 0;
		*cols = 0;
		string errMsg("In express version the total number of variables should be less or equal than 10 (");
		errMsg += string(lpszMatrixName)+").";
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return -1;
	}
	std::string this_header;
	if (!this_matrix->m_get_header_asstring(this_header)) {
		string errMsg = errorobjname+string(" ");
		errMsg += string(lpszMatrixName);
		errMsg += " has empty header";
		m_errorstr = errMsg;
		OnError(0xFFF1, errorobjname.c_str(), errMsg.c_str());
		return -1;
	}
	size_t szOfBuff = this_header.size() + 1;
	int header_num = *cols % (33/3);
	if((pHeaderBuf = (char*)malloc((szOfBuff + 1) * sizeof(char))) == NULL)
	{
		OnError(0xFFF1, errorobjname.c_str(), "There is insufficient memory available");
		return -1;
	}
	memset(pHeaderBuf, 0, szOfBuff + 1);
	this_header.copy(pHeaderBuf, szOfBuff);
	*cols = header_num;
	return static_cast<int>(szOfBuff) + 1;
}

int CRPSGSolverHelper::GetMatrixInfoSp( const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols, int* coeffs)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Getting Matrix Info", m_errorstr.c_str());
		return -1;
	}
	CPSGS_Data_Object* this_matrix = m_problem->m_getfrom_input_collection(string(lpszMatrixName), PSGS_PMATRIX);
	if (!this_matrix) {
		string errMsg("Matrix ");
		errMsg += string(lpszMatrixName);
		errMsg += " not found in internal collection";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return -1;
	}
	if (this_matrix->m_get_ndims() != 2) {
		string errMsg("Matrix ");
		errMsg += string(lpszMatrixName);
		errMsg += string(" has inconsistent data format");
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return -1;
	}
	*rows = this_matrix->m_get_size(0);
	*cols = this_matrix->m_get_size(1);
	*coeffs = this_matrix->m_get_nnz();

	int lmax = MAXVARNUMBER; // !!!Express!!!
	if (*cols>lmax) {
		*rows = 0;
		*cols = 0;
		string errMsg("In express version the total number of variables should be less or equal than 10 (");
		errMsg += string(lpszMatrixName)+string(").");
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return -1;
	}
	string this_header;
	if (!this_matrix->m_get_header_asstring(this_header)) {
		string errMsg("Matrix ");
		errMsg += string(lpszMatrixName);
		errMsg += " has empty header";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return -1;
	}
	int this_num = min(*cols, 40/4);
	size_t szOfBuff = this_header.size() + 1;
	if((pHeaderBuf = (char*)malloc((szOfBuff + 1) * sizeof(char))) == NULL)
	{
		OnError(0xFFF1, "Matrix", "There is insufficient memory available");
		return -1;
	}
	memset(pHeaderBuf, 0, szOfBuff + 1);
	this_header.copy(pHeaderBuf, szOfBuff);
	*cols = this_num;
	return static_cast<int>(szOfBuff) + 1;
}

bool CRPSGSolverHelper::GetMatrixData( const char* lpszMatrixName, double*& pData, int rows, int cols )
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Getting Matrix", m_errorstr.c_str());
		return false;
	}
	PSGS_DATA_OBJECT_TYPE thistype=PSGS_DATAOBJUNKNOWN;
	string locmatrixname(lpszMatrixName);
	string errorobjname;
	locmatrixname = strutil_Helper::RtoLower(locmatrixname);
	if (locmatrixname.find(string("matrix_"))==0) {
		thistype = PSGS_MATRIX;
		errorobjname = string("Matrix");
	}
	else if (locmatrixname.find(string("vector_"))==0) {
		thistype = PSGS_VECTOR;
		errorobjname = string("Vector");
	}
	else {
		string errMsg = string(lpszMatrixName);
		errMsg += " is not Matrix or Vector";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix//Vector", errMsg.c_str());
		return false;
	}
	CPSGS_Data_Object* this_matrix = m_problem->m_getfrom_input_collection(string(lpszMatrixName), thistype);
	if (!this_matrix) {
		string errMsg = errorobjname+" ";
		errMsg += string(lpszMatrixName);
		errMsg += " not found in internal collection";
		m_errorstr = errMsg;
		OnError(0xFFF1, errorobjname.c_str(), errMsg.c_str());
		return false;
	}
	if (this_matrix->m_get_ndims() != 2) {
		string errMsg = errorobjname+string(" ");
		errMsg += string(lpszMatrixName);
		errMsg += string(" has inconsistent data format");
		m_errorstr = errMsg;
		OnError(0xFFF1, errorobjname.c_str(), errMsg.c_str());
		return false;
	}
	if ((this_matrix->m_get_size(0) != rows) || (this_matrix->m_get_size(1) != cols)) {
		string errMsg = errorobjname+string(" ");
		errMsg += string(lpszMatrixName);
		errMsg += string(" has inconsistent data format");
		m_errorstr = errMsg;
		OnError(0xFFF1, errorobjname.c_str(), errMsg.c_str());
		return false;
	}
	int lmax = MAXVARNUMBER; // !!!Express!!!
	if (cols>lmax) {
		string errMsg("In express version the total number of variables should be less or equal than 10 (");
		errMsg += string(lpszMatrixName)+string(").");
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return -1;
	}
	pData = (double*)malloc(rows * cols * sizeof(double));
	double* pSaved = pData;
	if(pData == NULL) {
		OnError(0xFFF1, errorobjname.c_str(), "There is not enough memory to load matrix");
		return false;
	}
	int x_num = (int)ceil(cols/12)+cols;
	double* this_data = this_matrix->m_get_data();
	for (int64_t i=0; i<rows * x_num; i++) {
		*(pData++) = *(this_data++);
	}
	pData = pSaved;
	return true;
}

bool CRPSGSolverHelper::GetMatrixDataSp( const char* lpszMatrixName, double*& pData, int*& prows, int*& pcols, int coeffs)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Getting Matrix", m_errorstr.c_str());
		return false;
	}
	CPSGS_Data_Object* this_matrix = m_problem->m_getfrom_input_collection(string(lpszMatrixName), PSGS_PMATRIX);
	if (!this_matrix) {
		string errMsg("Matrix ");
		errMsg += string(lpszMatrixName);
		errMsg += " not found in internal collection";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return false;
	}
	if (this_matrix->m_get_ndims() != 2) {
		string errMsg("Matrix ");
		errMsg += string(lpszMatrixName);
		errMsg += string(" has inconsistent data format");
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return false;
	}
	if (this_matrix->m_get_nnz() != coeffs) {
		string errMsg("Matrix ");
		errMsg += string(lpszMatrixName);
		errMsg += " has inconsistent data format";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return false;
	}
	if (coeffs == 0) {
		prows = NULL;
		pcols = NULL;
		pData = NULL;
		return true;
	}
	pData = (double*)malloc(coeffs * sizeof(double));
	if(pData == NULL) {
		OnError(0xFFF1, "Matrix", "There is not enough memory to load matrix");
		return false;
	}
	prows = (int*)malloc(coeffs * sizeof(int));
	if(prows == NULL) {
		OnError(0xFFF1, "Matrix", "There is not enough memory to load matrix");
		return false;
	}
	pcols = (int*)malloc(coeffs * sizeof(int));
	if(pcols == NULL) {
		OnError(0xFFF1, "Matrix", "There is not enough memory to load matrix");
		return false;
	}
	double* this_data = this_matrix->m_get_data();
	for (int64_t i=0; i<coeffs; ++i) {
		prows[i] = (int)(*this_data++)-1;
		pcols[i] = (int)(*this_data++)-1;
		pData[i] = (double)(*this_data++);
	}
	return true;
}

void CRPSGSolverHelper::ReleaseBuffer( void* pBuffer )
{
	if (pBuffer) {
		free(pBuffer);
		pBuffer = NULL;
	}
}

void CRPSGSolverHelper::ReleaseMatrix(double* pMatrix)
{
	if (pMatrix) {
		free(static_cast<void*>(pMatrix));
		pMatrix= NULL;
	}
}

bool CRPSGSolverHelper::SaveObjective(char szObjectiveName[max_objectname_len], double value)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Objective", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Objective ");
		errMsg += string(szObjectiveName);
		errMsg += " was not evaluated";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Objective", errMsg.c_str());
		return false;
	}
	string objectivename;
	if (this_curr_problem->m_is_objective_in_output_fun_collection(objectivename)) {
		string errMsg("Objective ");
		errMsg += objectivename;
		errMsg += " is set already";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Objective", errMsg.c_str());
		return false;
	}
	CPSGS_Fun_Object* this_objective = new CPSGS_Fun_Object(string(szObjectiveName), PSGS_OBJECTIVE, PSGS_SCALAR_VALUE, string(""), value);
	this_curr_problem->m_add_newfunobject(&this_objective);
	return true;
}


bool CRPSGSolverHelper::SaveVars(char szVarsName[][max_name_len], double values[], unsigned nCount)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Variables", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Variables ");
		errMsg += " was not set";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Variables", errMsg.c_str());
		return false;
	}
	for (unsigned i=0; i<nCount; i++) {
		if (this_curr_problem->m_isin_output_fun_collection(string(szVarsName[i]))) {
			continue;
		}
		CPSGS_Fun_Object* this_object = new CPSGS_Fun_Object(string(szVarsName[i]), PSGS_FUNVAR, PSGS_SCALAR_VALUE, string(""), values[i]);
		this_curr_problem->m_add_newfunobject(&this_object);
	}
	return true;
}

bool CRPSGSolverHelper::SaveVector(const char* szVectorName, double varvalues[], unsigned nCount)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Vector", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Vector ");
		errMsg += string(szVectorName);
		errMsg += " was not set";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Vector", errMsg.c_str());
		return false;
	}
	if (this_curr_problem->m_isin_output_data_collection(string(szVectorName))) {
		string errMsg("Vector ");
		errMsg += string(szVectorName);
		errMsg += " coupling of the Object";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Vector", errMsg.c_str());
		return false;
	}
	vector<int64_t> size_arr;
	size_arr.resize(1); size_arr[0] = nCount; 
	CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(szVectorName), PSGS_VECTOR, NULL, &size_arr, varvalues);
	this_curr_problem->m_add_newdataobject(&this_object);
	return true;
}

bool CRPSGSolverHelper::SaveMatrix(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Matrix", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Matrix ");
		errMsg += string(szMatrixName);
		errMsg += " was not set";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return false;
	}
	int t_num = (int)ceil(numcol/11)+numcol;
	if (this_curr_problem->m_isin_output_data_collection(string(szMatrixName))) {
		string errMsg("Matrix ");
		errMsg += string(szMatrixName);
		errMsg += " coupling of the Object";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Matrix", errMsg.c_str());
		return false;
	}
	vector<int64_t> size_arr;
	size_arr.resize(2); size_arr[0] = numrow; size_arr[1] = t_num;
	vector<string> header_arr;
	for (int64_t i=0; i<numcol; i++){
		header_arr.push_back(string(szVarsName[i]));
	}
	CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(szMatrixName), PSGS_MATRIX, &header_arr, &size_arr, matrixdata);
	this_curr_problem->m_add_newdataobject(&this_object);
	return true;
}

bool CRPSGSolverHelper::SavePoint(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Point", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Point ");
		errMsg += string(szPointName);
		errMsg += " was not set";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Point", errMsg.c_str());
		return false;
	}
	if (this_curr_problem->m_isin_output_data_collection(string(szPointName))) {
		string errMsg("Point ");
		errMsg += string(szPointName);
		errMsg += " coupling of the Object";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Point", errMsg.c_str());
		return false;
	}
	vector<int64_t> size_arr;
	size_arr.resize(2); size_arr[0] = nCount; size_arr[1] = 1;
	vector<string> header_arr;
	for (int64_t i=0; i<nCount; i++){
		header_arr.push_back(string(szVarsName[i]));
	}
	size_arr[0] = (int64_t)min((int)nCount, 13);
	CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(szPointName), PSGS_POINT, &header_arr, &size_arr, values);
	this_curr_problem->m_add_newdataobject(&this_object);
	return true;
}

bool CRPSGSolverHelper::AddPoint(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Add Point", m_errorstr.c_str());
		return false;
	}
	if (m_problem->m_isin_input_collection(string(szPointName))) {
		CPSGS_Data_Object* thisobject = m_problem->m_getfrom_input_collection(string(szPointName));
		if (thisobject)	{
			m_problem->m_removefrom_input_collection(thisobject);
			delete thisobject;
		}
		else {
			string errMsg("Point ");
			errMsg += string(szPointName);
			errMsg += ". Coupling of the Object. Cannot remome old object.";
			OnError(0xFFF1, "Point", errMsg.c_str());
			return false;
		}
		string errMsg("Point ");
		errMsg += string(szPointName);
		errMsg += ". Coupling of the Object. It will be replaced";
		OnWarning(0, "Point", errMsg.c_str());
	}
	vector<int64_t> size_arr;
	size_arr.resize(2); size_arr[0] = nCount; size_arr[1] = 1;
	vector<string> header_arr;
	for (int64_t i=0; i<nCount; i++){
		header_arr.push_back(string(szVarsName[i]));
	}
	CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(szPointName), PSGS_POINT, &header_arr, &size_arr, values);
	m_problem->m_add_newdataobject(&this_object);
	if (this_object->m_get_size(0) > 11) return false;
	return true;
}

bool CRPSGSolverHelper::AddVector(const char* szVectorName, double varvalues[], unsigned nCount)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Add Vector", m_errorstr.c_str());
		return false;
	}
	if (m_problem->m_isin_input_collection(string(szVectorName))) {
		CPSGS_Data_Object* thisobject = m_problem->m_getfrom_input_collection(string(szVectorName));
		if (thisobject)	{
			m_problem->m_removefrom_input_collection(thisobject);
			delete thisobject;
		}
		else {
			string errMsg("Vector ");
			errMsg += string(szVectorName);
			errMsg += ". Coupling of the Object. Cannot remome old object.";
			OnError(0xFFF1, "Vector", errMsg.c_str());
			return false;
		}
		string errMsg("Vector ");
		errMsg += string(szVectorName);
		errMsg += ". Coupling of the Object. It will be replaced";
		OnWarning(0, "Vector", errMsg.c_str());
	}
	vector<int64_t> size_arr;
	size_arr.resize(1); size_arr[0] = nCount;
	CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(szVectorName), PSGS_VECTOR, NULL, &size_arr, varvalues);
	m_problem->m_add_newdataobject(&this_object);
	return true;
}

bool CRPSGSolverHelper::AddMatrix(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Add Matrix", m_errorstr.c_str());
		return false;
	}
	if (m_problem->m_isin_input_collection(string(szMatrixName))) {
		CPSGS_Data_Object* thisobject = m_problem->m_getfrom_input_collection(string(szMatrixName));
		if (thisobject)	{
			m_problem->m_removefrom_input_collection(thisobject);
			delete thisobject;
		}
		else {
			string errMsg("Matrix ");
			errMsg += string(szMatrixName);
			errMsg += ". Coupling of the Object. Cannot remome old object.";
			OnError(0xFFF1, "Matrix", errMsg.c_str());
			return false;
		}
		string errMsg("Matrix ");
		errMsg += string(szMatrixName);
		errMsg += ". Coupling of the Object. It will be replaced";
		OnWarning(0, "Matrix", errMsg.c_str());
	}
	vector<int64_t> size_arr;
	size_arr.resize(2); size_arr[0] = numrow; size_arr[1] = numcol;
	vector<string> header_arr;
	for (int64_t i=0; i<numcol; i++){
		header_arr.push_back(string(szVarsName[i]));
	}
	CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(szMatrixName), PSGS_MATRIX, &header_arr, &size_arr, matrixdata);
	m_problem->m_add_newdataobject(&this_object);
	if (this_object->m_get_size(1) > 12) return false;
	return true;
}

bool CRPSGSolverHelper::SaveObjs(char szObjName[][max_objectname_len], double val[], char szConstrName[][max_name_len], char type[], unsigned nCount)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Objects", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Objects ");
		errMsg += " was not evaluated";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Objects", errMsg.c_str());
		return false;
	}
	this_curr_problem->m_erase_funobjects(PSGS_FUNCTION);
	PSGS_SCALAR_VECTOR_TYPE this_sclvec_type;
	for (unsigned i=0; i<nCount; i++) {
		if (this_curr_problem->m_isin_output_fun_collection(string(szObjName[i]))) {
			continue;
		}
		switch (type[i]) {
			case 'S': this_sclvec_type = PSGS_SCALAR_VALUE;	break;
			case 'V': this_sclvec_type = PSGS_VECTOR_VALUE;	break;
			case 'E': this_sclvec_type = PSGS_EMPTY_VALUE;	break;
			default: this_sclvec_type = PSGS_SCVALVECUNKNOWN; break;
		}
		CPSGS_Fun_Object* this_object = new CPSGS_Fun_Object(string(szObjName[i]), PSGS_FUNCTION, this_sclvec_type, string(szConstrName[i]), val[i]);
		this_curr_problem->m_add_newfunobject(&this_object);
	}
	return true;
}

bool CRPSGSolverHelper::SaveConstraints(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount )
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Constraints", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Constraints ");
		errMsg += " was not evaluated";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Constraints", errMsg.c_str());
		return false;
	}
	this_curr_problem->m_erase_funobjects(PSGS_CONSTRAINT);
	PSGS_SCALAR_VECTOR_TYPE this_sclvec_type;
	for (unsigned i=0; i<nCount; i++) {
		if (this_curr_problem->m_isin_output_fun_collection(string(szConstrName[i]))) {
			continue;
		}
		switch (type[i]) {
			case 'S': this_sclvec_type = PSGS_SCALAR_VALUE;	break;
			case 'V': this_sclvec_type = PSGS_VECTOR_VALUE;	break;
			case 'E': this_sclvec_type = PSGS_EMPTY_VALUE;	break;
			default: this_sclvec_type = PSGS_SCVALVECUNKNOWN; break;
		}
		CPSGS_Fun_Object* this_object = new CPSGS_Fun_Object(string(szConstrName[i]), PSGS_CONSTRAINT, this_sclvec_type, string(""), val[i]);
		this_curr_problem->m_add_newfunobject(&this_object);
	}
	return true;
}

bool CRPSGSolverHelper::SaveConstraintsSlack(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount )
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Constraints Slacks", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Constraints Slacks");
		errMsg += " was not evaluated";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Constraints Slacks", errMsg.c_str());
		return false;
	}
	this_curr_problem->m_erase_funobjects(PSGS_CONSTRAINT_SLACK);
	PSGS_SCALAR_VECTOR_TYPE this_sclvec_type;
	for (unsigned i=0; i<nCount; i++) {
		if (!this_curr_problem->m_isin_output_fun_collection(string(szConstrName[i]))) {
			continue;
		}
		switch (type[i]) {
			case 'S': this_sclvec_type = PSGS_SCALAR_VALUE;	break;
			case 'V': this_sclvec_type = PSGS_VECTOR_VALUE;	break;
			case 'E': this_sclvec_type = PSGS_EMPTY_VALUE;	break;
			default: this_sclvec_type = PSGS_SCVALVECUNKNOWN; break;
		}
		CPSGS_Fun_Object* this_object = new CPSGS_Fun_Object(string(szConstrName[i]), PSGS_CONSTRAINT_SLACK, this_sclvec_type, string(szConstrName[i]), val[i]);
		this_curr_problem->m_add_newfunobject(&this_object);
	}
	return true;
}

bool CRPSGSolverHelper::SaveStatus( const char* szStatus )
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Status", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Save Status");
		errMsg += " was not evaluated";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Save Status", errMsg.c_str());
		return false;
	}
	this_curr_problem->m_set_solution_status(string(szStatus));
	return true;
}

int CRPSGSolverHelper::OnCancel( void )
{
#ifdef WIN32
	if (GetAsyncKeyState(VK_CANCEL) != 0) {
		m_set_whattodo(HELPER_REGIME_INTERRUPT_R);
		if (m_ProcessCancelToRFun) {
			m_ProcessCancelToRFun();
		}
		return 1;
	}
	else {
		return 0;
	}
	return m_nCancelStatus;
#elif LINUX
	m_nCancelStatus = 0;
	return m_nCancelStatus;
#else
#error Platform not supported
#endif
}

int CRPSGSolverHelper::OnFinish( void )
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "On Finish", m_errorstr.c_str());
		return -1;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("On Finish");
		errMsg += " was not evaluated";
		m_errorstr = errMsg;
		OnError(0xFFF1, "On Finish", errMsg.c_str());
		return -1;
	}
	if ((m_whattodo == HELPER_REGIME_UNKNOWN_R) || (m_whattodo == HELPER_REGIME_INTERRUPT_R)) {
		m_problem->m_clear_output_objects();
		if (m_ProcessStopToRFun) {
			m_ProcessStopToRFun(NULL, 0);
		}
		return 1;
	}
	if (m_get_whattodo() == HELPER_REGIME_INTERRUPT_R) {
		return 1;
	}
	vector<string>* thisoutput = this_curr_problem->m_get_output();
	if (!thisoutput->empty()) {
		for (vector<string>::iterator it_out = thisoutput->begin(); it_out != thisoutput->end(); it_out++) {
			if (m_PrintToRFun) {
				m_PrintToRFun(it_out->c_str(), WHATPRINT_MESSAGE_R);
			}
		}
	}
	if (m_problem->m_is_problem_regular()) {
		m_set_whattodo(HELPER_REGIME_SOLVED_R);
		if (m_ProcessStopToRFun) {
			m_ProcessStopToRFun(NULL, 0);
		}
	}
	return 1;
}

int CRPSGSolverHelper::OnCycleFinish(void)
{
	if (m_key_block_newoutputproblem) return 1;
	m_set_whattodo(HELPER_REGIME_SOLVED_R);
	if (m_CycleFinishToRFun) {
		m_CycleFinishToRFun(NULL, 0);
	}
	return 1;
}

void CRPSGSolverHelper::GetRootPath( char* pBuffer )
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Get Root Path", m_errorstr.c_str());
		return;
	}
	memset(pBuffer, 0, MAX_PATH);
	strcpy(pBuffer, m_problem->m_get_root_path().c_str());
}

int CRPSGSolverHelper::SaveProblemName( char* pBuffer )
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem Name was not set");
		OnError(0xFFF1, "Save Problem Name", m_errorstr.c_str());
		return -1;
	}
	if (m_problem->m_get_name().empty()) {
		m_problem->m_set_name(string(pBuffer));
	}
	if (m_key_block_newoutputproblem) return 1;
	if (m_problem->m_is_problem_regular()) {
		vector<CPSGS_OutputProblem*>* this_output_collection = m_problem->m_get_output_problem_collection();
		if (!this_output_collection->empty()) {
			if (this_output_collection->size() > 1) {
				m_errorstr = string("Problem is not Regular");
				OnError(0xFFF1, "Save Problem Name", m_errorstr.c_str());
				return -1;
			}
			if (this_output_collection->at(0)->m_get_name() != string(pBuffer)) {
				this_output_collection->at(0)->m_set_name(string(pBuffer));
				m_problem->m_set_curr_output_problem(this_output_collection->at(0));
				return 1;
			}
		}
		else {
			CPSGS_OutputProblem* this_new_output = new CPSGS_OutputProblem();
			this_new_output->m_set_name(string(pBuffer));
			m_problem->m_set_curr_output_problem(this_new_output);
			this_output_collection->push_back(this_new_output);
		}
		return 1;
	}
	else { // multi
		vector<CPSGS_OutputProblem*>* this_output_collection = m_problem->m_get_output_problem_collection();
		if (!this_output_collection->empty()) {
			vector<CPSGS_OutputProblem*>::iterator it_output = find(this_output_collection->begin(), this_output_collection->end(), string(pBuffer));
			if (it_output != this_output_collection->end()) {
				CPSGS_OutputProblem* this_output = *it_output;
				this_output->m_set_name(string(pBuffer));
				m_problem->m_set_curr_output_problem(this_output);
				return 1;
			}
		}
		CPSGS_OutputProblem* this_new_output = new CPSGS_OutputProblem();
		this_new_output->m_set_name(string(pBuffer));
		m_problem->m_set_curr_output_problem(this_new_output);
		this_output_collection->push_back(this_new_output);
	}
	return 1;
}

int CRPSGSolverHelper::SaveProblemStatement(char* problemstatement)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem Statement was not set");
		OnError(0xFFF1, "Problem Statement", m_errorstr.c_str());
		return false;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Problem Statement");
		errMsg += " was not evaluated";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Problem Statement", errMsg.c_str());
		return false;
	}
	this_curr_problem->m_set_statement(string(problemstatement));
	return 1;
}

int CRPSGSolverHelper::SaveSolution(char* problemsolution)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem Solution was not saved");
		OnError(0xFFF1, "Problem Solution", m_errorstr.c_str());
		return -1;
	}
	CPSGS_OutputProblem* this_curr_problem = m_problem->m_get_curr_output_problem();
	if (this_curr_problem==NULL) {
		string errMsg("Problem Solution");
		errMsg += " was not saved";
		m_errorstr = errMsg;
		OnError(0xFFF1, "Problem Solution", errMsg.c_str());
		return -1;
	}
	this_curr_problem->m_set_output_from_string(string(problemsolution));
	return 1;
}

void CRPSGSolverHelper::OnProcessStop(int nErrCode)
{
	if (m_ProcessStopToRFun) {
		m_ProcessStopToRFun(NULL, TEG_PROCESSSTOPNOW);
	}
	return;
}

int CRPSGSolverHelper::SetMultyProblem(int ismulty)
{
	if (m_problem==NULL) {
		m_errorstr = string("Problem was not set");
		OnError(0xFFF1, "Save Status", m_errorstr.c_str());
		return -1;
	}
	if (m_key_block_newoutputproblem) return 1;
	m_problem->m_set_problem_struct_type(ismulty==1?CICLYC__PROBLEM_STRUCT_TYPE:REGULAR_PROBLEM_STRUCT_TYPE);
	if (m_problem->m_get_problem_struct_type() == REGULAR_PROBLEM_STRUCT_TYPE) { // needs to add output
		vector<CPSGS_OutputProblem*>* this_output_collection = m_problem->m_get_output_problem_collection();
		if (!this_output_collection->empty()) {
			if (this_output_collection->size() > 1) {
				m_errorstr = string("Problem is not Regular");
				OnError(0xFFF1, "Set Problem structure", m_errorstr.c_str());
				return -1;
			}
			else {
				m_problem->m_set_curr_output_problem(this_output_collection->at(0));
				return 1;
			}
		}
		else {
			CPSGS_OutputProblem* this_new_output = new CPSGS_OutputProblem();
			this_new_output->m_set_name(m_problem->m_get_name());
			this_new_output->m_set_statement(m_problem->m_get_statement());
			m_problem->m_set_curr_output_problem(this_new_output);
			this_output_collection->push_back(this_new_output);
		}
	}
	return 0;
}


int CRPSGSolverHelper::ImportProblemFromTextFiles(std::string pathtofile, CRPSGSolverHelper* thisImpl)
{
	string errorstr;
	CPSGS_Problem* this_problem = thisImpl->m_get_problem();
	if (this_problem) {
		this_problem->m_clear_problem();
	}
	else {
		this_problem = new CPSGS_Problem();
		thisImpl->m_set_problem(this_problem);
	}
	string root_path, file_name, problem_name;
	if (!get_path_from_filename(pathtofile, root_path, file_name)) {
		this_problem->m_clear_problem();
		errorstr = string("Cannot get root path");
		this_problem->m_set_error(errorstr);

		char pBuffer[1024];
		memset(pBuffer, 0, MAX_PATH);
		if (getcwd(pBuffer, MAX_PATH)==NULL)
			return -1;
		this_problem->m_set_root_path(string(pBuffer));

		thisImpl->OnError(0xFFF1, "Load Problem from text", errorstr.c_str());
		return -1;
	}
	this_problem->m_set_root_path(root_path);
	string statement;
	int res = load_problem_statement_from_text(pathtofile, statement, problem_name, errorstr);
	if (res <= 0) {
		this_problem->m_clear_problem();
		this_problem->m_set_error(errorstr);
		thisImpl->OnError(0xFFF1, "Load Problem from text", errorstr.c_str());
		return -1;
	}
	this_problem->m_set_statement(statement);
	CPSGSolver solver;
	thisImpl->Init();
	thisImpl->m_set_block_newoutputproblem(true);
	res = solver.SInit(thisImpl);
	if (res != 0) {
		thisImpl->End();
		return -1;
	}
	int count = 0;
	while (++count <= 50) {
		if (!(this_problem->m_get_name().empty())) {
			break;
		}
		sleep(100);
	}
	thisImpl->m_set_block_newoutputproblem(false);
	if (count>=50) {
		this_problem->m_set_name("problem_noname");
	}
	thisImpl->End();
	vector<string> point_arr, vector_arr, matrix_arr, pmatrix_arr, added_objects_arr;
	if (GetPSGDataObjectsFromStatement(statement, thisImpl, point_arr, vector_arr, matrix_arr, pmatrix_arr, added_objects_arr, errorstr)<0) {
		this_problem->m_clear_problem();
		this_problem->m_set_error(errorstr);
		thisImpl->OnError(0xFFF1, "Load Problem from text", errorstr.c_str());
		return -1;
	}
	char pathtoobjfile[1024];
	if (matrix_arr.size()>0) {
		for(vector<string>::iterator it_name=matrix_arr.begin(); it_name!=matrix_arr.end(); it_name++) {
			sprintf(pathtoobjfile, "%s/%s.txt", root_path.c_str(), it_name->c_str());
			double* matrix_body;
			vector<string> header_arr;
			int64_t n_rows;
			int64_t n_cols;
			string errorstrloc;
			if (read_matrix_fromtext(string(pathtoobjfile),
					header_arr, &matrix_body, n_rows, n_cols, errorstrloc)<0) {
				this_problem->m_clear_problem();
				errorstr = *it_name + string(": ") + errorstrloc;
				this_problem->m_set_error(errorstr);
				thisImpl->OnError(0xFFF1, "Load Problem from text", errorstr.c_str());
				return -1;
			}
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = n_rows; size_arr[1] = n_cols;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(*it_name, PSGS_MATRIX, &header_arr, &size_arr, matrix_body);
			this_problem->m_add_newdataobject(&this_object);
			delete [] matrix_body;
		}
	}
	if (pmatrix_arr.size()>0) {
		for(vector<string>::iterator it_name=pmatrix_arr.begin(); it_name!=pmatrix_arr.end(); it_name++) {
			sprintf(pathtoobjfile, "%s/%s.txt", root_path.c_str(), it_name->c_str());
			double* matrix_body;
			vector<string> header_arr;
			int64_t n_rows, n_cols, n_nz;
			string errorstrloc;
			if (read_pmatrix_fromtext(string(pathtoobjfile), header_arr, &matrix_body, n_rows, n_cols, n_nz, errorstrloc)<0) {
				this_problem->m_clear_problem();
				errorstr = *it_name + string(": ") + errorstrloc;
				this_problem->m_set_error(errorstr);
				thisImpl->OnError(0xFFF1, "Load Problem from text", errorstr.c_str());
				return -1;
			}
			vector<int64_t> size_arr;
			size_arr.resize(3); size_arr[0] = n_rows; size_arr[1] = n_cols; size_arr[2] = n_nz;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(*it_name, PSGS_PMATRIX, &header_arr, &size_arr, matrix_body);
			this_problem->m_add_newdataobject(&this_object);
			delete [] matrix_body;
		}
	}
	if (vector_arr.size()>0) {
		for(vector<string>::iterator it_name=vector_arr.begin(); it_name!=vector_arr.end(); it_name++) {
			sprintf(pathtoobjfile, "%s/%s.txt", root_path.c_str(), it_name->c_str());
			double* matrix_body;
			vector<string> header_arr;
			int64_t n_rows;
			string errorstrloc;
			if (read_vector_fromtext(string(pathtoobjfile), header_arr, &matrix_body, n_rows, errorstrloc)<0) {
				this_problem->m_clear_problem();
				errorstr = *it_name + string(": ") + errorstrloc;
				this_problem->m_set_error(errorstr);
				thisImpl->OnError(0xFFF1, "Load Problem from text", errorstr.c_str());
				return -1;
			}
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = n_rows; size_arr[1] = 2;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(*it_name, PSGS_VECTOR, &header_arr, &size_arr, matrix_body);
			this_problem->m_add_newdataobject(&this_object);
			delete [] matrix_body;
		}
	}
	if (point_arr.size()>0) {
		for(vector<string>::iterator it_name=point_arr.begin(); it_name!=point_arr.end(); it_name++) {
			sprintf(pathtoobjfile, "%s/%s.txt", root_path.c_str(), it_name->c_str());
			double* matrix_body;
			vector<string> header_arr;
			int64_t n_rows;
			string errorstrloc;
			if (read_point_fromtext(string(pathtoobjfile), header_arr, &matrix_body, n_rows, errorstrloc)<0) {
				this_problem->m_clear_problem();
				errorstr = *it_name + string(": ") + errorstrloc;
				this_problem->m_set_error(errorstr);
				thisImpl->OnError(0xFFF1, "Load Problem from text", errorstr.c_str());
				return -1;
			}
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = n_rows; size_arr[1] = 1;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(*it_name, PSGS_POINT, &header_arr, &size_arr, matrix_body);
			this_problem->m_add_newdataobject(&this_object);
			delete [] matrix_body;
		}
	}
	return 1;
}

struct vectortype {
	vector<string>* vec;
	PSGS_DATA_OBJECT_TYPE type;
	vectortype(vector<string>* avec, PSGS_DATA_OBJECT_TYPE atype) {vec = avec; type = atype;}
	vectortype(void) {vec = NULL; type = PSGS_DATAOBJUNKNOWN;}
	vectortype(const vectortype &old) {vec = old.vec; type = old.type;}
};


int CRPSGSolverHelper::ExportProblemToTextFiles(std::string pathtofile, CRPSGSolverHelper* thisImpl)
{
	CPSGS_Problem* this_problem = thisImpl->m_get_problem();
	string errorstr;
	if (!this_problem) {
		errorstr = string("Problem not found");
		this_problem->m_set_error(errorstr);
		thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
		return -1;
	}
	string statement = this_problem->m_get_statement();
	if (statement.empty()) {
		errorstr = string("Problem statement is empty");
		this_problem->m_set_error(errorstr);
		thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
		return -1;
	}
	vector<string> point_arr, vector_arr, matrix_arr, pmatrix_arr, added_objects_arr;
	thisImpl->OnMessage(0xFFF1, (char*)"Export Problem to text");
	thisImpl->OnMessage(0xFFF1, (char*)"Export Problem Statement");
	if (GetPSGDataObjectsFromStatement(statement, thisImpl, point_arr, vector_arr, matrix_arr, pmatrix_arr, added_objects_arr, errorstr)<0) {
		errorstr = string("Problem statement is empty");
		this_problem->m_set_error(errorstr);
		thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
		return -1;
	}
	string root_path, file_name, problem_name;
	if (!get_path_from_filename(pathtofile, root_path, file_name)) {
		errorstr = string("Wrong Export path");
		this_problem->m_set_error(errorstr);
		thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
		return -1;
	}
#ifdef WIN32
	WIN32_FIND_DATA FindFileData;
	HANDLE hFind;
	hFind = FindFirstFile((root_path+"\\*.*").c_str(), &FindFileData);
	if (hFind == INVALID_HANDLE_VALUE) {
		if (!CreateDirectory(root_path.c_str(), NULL)) { // create new folder
			errorstr = "Export folder: "+root_path+" cannot be created";
			this_problem->m_set_error(errorstr);
			thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
			return -1;
		}
	}
#elif LINUX
	DIR *workdir = opendir(root_path.c_str());
	if (workdir == NULL) {
		const int dir_error = mkdir(root_path.c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
		if (dir_error == -1) {
			errorstr = "Export folder: "+root_path+" cannot be created";
			this_problem->m_set_error(errorstr);
			thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
			return -1;
		}
	}
	closedir(workdir);
#else
#error Platform not supported
#endif
	string errorstrloc;
	char pathtostatfile[1024];
	sprintf(pathtostatfile, "%s/%s", root_path.c_str(), file_name.c_str());
	if (CPSGS_Problem::StoreStatementInTextFile(pathtostatfile, this_problem, errorstrloc)<0) {
		errorstr = this_problem->m_get_name() + string(": ") + errorstrloc;
		this_problem->m_set_error(errorstr);
		thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
		return -1;
	}
	char pathtoobjfile[1024];
	vector<vectortype> thisfullobj_arr;
	if (matrix_arr.size()>0) {
		vectortype thisvectype;
		thisfullobj_arr.push_back(thisvectype);
		thisfullobj_arr.back().vec = &matrix_arr; thisfullobj_arr.back().type = PSGS_MATRIX;
	}
	if (pmatrix_arr.size()>0) {
		vectortype thisvectype;
		thisfullobj_arr.push_back(thisvectype);
		thisfullobj_arr.back().vec = &pmatrix_arr; thisfullobj_arr.back().type = PSGS_PMATRIX;
	}
	if (point_arr.size()>0) {
		vectortype thisvectype;
		thisfullobj_arr.push_back(thisvectype);
		thisfullobj_arr.back().vec = &point_arr; thisfullobj_arr.back().type = PSGS_POINT;
	}
	if (vector_arr.size()>0) {
		vectortype thisvectype;
		thisfullobj_arr.push_back(thisvectype);
		thisfullobj_arr.back().vec = &vector_arr; thisfullobj_arr.back().type = PSGS_VECTOR;
	}
	thisImpl->OnMessage(0xFFF1, (char*)"Export Data");
	for (vector<vectortype>::iterator it_type = thisfullobj_arr.begin(); it_type != thisfullobj_arr.end(); it_type++) {
		vector<string>* obj_arr = it_type->vec;
		PSGS_DATA_OBJECT_TYPE obj_type = it_type->type;
		if (obj_arr->size()>0) {
			for(vector<string>::iterator it_name=obj_arr->begin(); it_name!=obj_arr->end(); it_name++) {
				CPSGS_Data_Object* this_object = this_problem->m_getfrom_input_collection(*it_name, obj_type);
				if (this_object==NULL) {
					errorstr = *it_name + string(": not found");
					this_problem->m_set_error(errorstr);
					thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
					return -1;
				}
				thisImpl->OnMessage(0xFFF1, (char*)(string("Export ")+(*it_name)).c_str());
				sprintf(pathtoobjfile, "%s/%s.txt", root_path.c_str(), it_name->c_str());
				if (CPSGS_Data_Object::StoreInTextFile(pathtoobjfile, this_object, errorstrloc)<0) {
					errorstr = *it_name + string(": ") + errorstrloc;
					this_problem->m_set_error(errorstr);
					thisImpl->OnError(0xFFF1, "Export Problem to text", errorstr.c_str());
					return -1;
				}
			}
		}
	}
	return 1;
}

int CRPSGSolverHelper::GetPSGDataObjectsFromStatementDirect(std::string statement, CRPSGSolverHelper* thisImpl, std::string& firstterm, std::string& secondterm,
	std::vector<std::string>& point_arr, std::vector<std::string>& vector_arr, std::vector<std::string>& matrix_arr, std::vector<std::string>& pmatrix_arr, 
		std::string& errorstr)
{
	errorstr.clear(); secondterm.clear(); firstterm.clear(); point_arr.clear(); vector_arr.clear(); matrix_arr.clear(); pmatrix_arr.clear();
	vector<string> body_str_arr, added_objects_arr;
	body_str_arr = strutil_Helper::split(statement, string(" ()*-=,\t\r\n;[]"), false);
	if (body_str_arr.empty()) {
		errorstr = string("Description is empty");
		return 0;
	}
	if (get_PSG_objects_from_arr(&body_str_arr, point_arr, vector_arr, matrix_arr, pmatrix_arr, added_objects_arr)==0) {
		errorstr = string("Description is empty");
		return 0;
	}
	firstterm = body_str_arr[0];
	if (body_str_arr.size()>1) {
		string secloc = body_str_arr[1];
		if (!isdigit(secloc.c_str()[0])) {
			if ((strutil_Helper::RtoLower(secloc).find("point_")!=0) && (strutil_Helper::RtoLower(secloc).find("vector_")!=0) && (strutil_Helper::RtoLower(secloc).find("matrix_")!=0) && (strutil_Helper::RtoLower(secloc).find("pmatrix_")!=0)) {
				secondterm = secloc;
			}
		}
	}
	return 1;
}

std::string CRPSGSolverHelper::remove_comments_fromstring(std::string thisstr)
{
	string resstr;
	vector<string> string_arr;
	if (thisstr.empty()) return thisstr;
	int len = (int)thisstr.size();
	char *buffer = (char*) malloc((len+1) * sizeof(char));
	memset(buffer, 0, sizeof(char) * (len+1));
	strcpy(buffer, thisstr.c_str());
	char seps[]   = "\r\n";
	char *token, *next_token;
	token = strutil_Helper::strtok_uni(buffer, seps, &next_token);
	while (token != NULL)	{
		strutil_Helper::chartrim(token, "\t ");
		if (strlen(token) != 0) {
			char* ptrch = strchr(token, '%');
			if (ptrch != NULL) {
				if (ptrch != token) {
					*ptrch = '\0';
					strutil_Helper::chartrim(token, "\t ");
				}
				else {
					token = strutil_Helper::strtok_uni(NULL, seps, &next_token);
					continue;
				}
			}
			resstr += string(token) + string("\n");
		}
		token = strutil_Helper::strtok_uni(NULL, seps, &next_token);
	}
	delete [] buffer;
	return resstr;
}


int CRPSGSolverHelper::GetPSGDataObjectsFromStatement(std::string statement, CRPSGSolverHelper* thisImpl, 
	std::vector<std::string>& point_arr, std::vector<std::string>& vector_arr, std::vector<std::string>& matrix_arr, std::vector<std::string>& pmatrix_arr, std::vector<std::string>& added_objects_arr, 
		std::string& errorstr)
{
	point_arr.clear(); vector_arr.clear(); matrix_arr.clear(); pmatrix_arr.clear(); added_objects_arr.clear();
	vector<string> body_str_arr;
	int problem_count = 1;
	int initoutbufflen = 4096;
	char* outBuffer;
	int count_tmp = 0;
	int res;
	errorstr.clear();
	statement = remove_comments_fromstring(statement);
	while (1) {
		int outbuflen = initoutbufflen;
		outBuffer = new char[outbuflen*sizeof(char)];
		memset(outBuffer, 0, sizeof(char)*outbuflen);
		while (1) {
			getoneproblemfromcycle_psg((char*)statement.c_str(), (int)statement.size()+1, problem_count, outBuffer, outbuflen, &res, thisImpl);
			if (res == -1) {
				free(outBuffer);
				errorstr = string("PSG Data Objects not found");
				return -1;
			}
			if (res < -1) {
				if (++count_tmp>10) {
					free(outBuffer);
					errorstr = string("PSG Data Objects not found");
					return -1;
				}
				free(outBuffer);
				outbuflen *= 2;
				outBuffer = new char[outbuflen*sizeof(char)];
				memset(outBuffer, 0, sizeof(char)*outbuflen);
				continue;
			}
			break;
		}
		if (res==0) {
			free(outBuffer);
			break;
		}
		if (res==2) {
			free(outBuffer);
			problem_count++;
			continue;
		}
		body_str_arr = strutil_Helper::split(string(outBuffer), string(" ()*-=,\t\r\n;[]"), false);
		if (body_str_arr.empty()) {
			free(outBuffer);
			problem_count++;
			continue;
		}
		if (get_PSG_objects_from_arr(&body_str_arr, point_arr, vector_arr, matrix_arr, pmatrix_arr, added_objects_arr)==0) {
			free(outBuffer);
			problem_count++;
			continue;
		}
		free(outBuffer);
		problem_count++;
	}
	return 1;
}

int CRPSGSolverHelper::get_Function_Value_1(CRPSGSolverHelper* thisImpl, const char* pBuffer, const char* pPointName, double* fvalue)
{
	if(thisImpl == NULL) return 0;
	return vk_getfunctionvalue(pBuffer, pPointName, fvalue, thisImpl);
}

int CRPSGSolverHelper::get_Function_Gradient_1(CRPSGSolverHelper* thisImpl, const char* pBuffer, const char* pPointName, CPSGS_Data_Object** pp_this_point, double* fValue)
{
	if(thisImpl == NULL) return 0;
	int res= vk_getfunctiongradient(pBuffer, pPointName, fValue, thisImpl);
	if (res != 0) {
		thisImpl->OnError(0, "PSG Function Sensetivity", "Error while calculations");
		return res;
	}
	CPSGS_Problem* thisproblem = thisImpl->m_get_problem();
	if (thisproblem==NULL) return -1;
	CPSGS_OutputProblem* thiscurruotproblem = thisproblem->m_get_curr_output_problem();
	if (thiscurruotproblem==NULL) return -1;
	CPSGS_Data_Object* thisobject = thiscurruotproblem->m_get_fromoutput_data_collection(string("point_gradient"));
	if (thisobject == NULL) {
		thisImpl->OnError(0, "PSG Function Sensetivity", "Output Point not found");
		return -1;
	}
	*pp_this_point = thisobject;
	return 0;
}

int CRPSGSolverHelper::get_Function_Increment_1(CRPSGSolverHelper* thisImpl, const char* pBuffer, const char* pPointName, CPSGS_Data_Object** pp_this_point, double* fValue)
{
	if(thisImpl == NULL) return 0;
	int res= vk_getfunctionincrement(pBuffer, pPointName, fValue, thisImpl);
	if (res != 0) {
		thisImpl->OnError(0, "PSG Function Increment", "Error while calculations");
		return res;
	}
	CPSGS_Problem* thisproblem = thisImpl->m_get_problem();
	if (thisproblem==NULL) return -1;
	CPSGS_OutputProblem* thiscurruotproblem = thisproblem->m_get_curr_output_problem();
	if (thiscurruotproblem==NULL) return -1;
	CPSGS_Data_Object* thisobject = thiscurruotproblem->m_get_fromoutput_data_collection(string("point_increment"));
	if (thisobject == NULL) {
		thisImpl->OnError(0, "PSG Function Increment", "Output Point not found");
		return -1;
	}
	*pp_this_point = thisobject;
	return 0;
}

int CRPSGSolverHelper::run_rpsg_solver(CRPSGSolverHelper* thisImpl)
{
	if(thisImpl == NULL) return -1;
	int iSolver = 1;
	int nStage = 3;
	double Accuracy = 12;
	CPSGS_Problem* thisproblem = thisImpl->m_get_problem();
	thisproblem->m_clear_output_objects();
	thisproblem->m_set_problem_struct_type(UNKNOWN_PROBLEM_STRUCT_TYPE);
	thisImpl->m_set_whattodo(HELPER_REGIME_SOLVE_R); // solve
	CPSGSolver solver;
	thisImpl->Init();
	solver.SetInheritKey(0);
	if (thisproblem->m_get_name().empty()) {
		thisImpl->m_set_block_newoutputproblem(true);
		int res = solver.SInit(thisImpl);
		if (res != 0) {
			thisImpl->End();
			return 1;
		}
		int count = 0;
		while (++count <= 50) {
			if (!(thisproblem->m_get_name().empty())) {
				break;
			}
			sleep(100);
		}
		thisImpl->m_set_block_newoutputproblem(false);
		if (count>=50) {
			thisImpl->OnWarning(0, "PSG Solver", "Problem Name not defined");
			return 1;
		}
	}
	thisImpl->m_set_whattodo(HELPER_REGIME_SOLVE_R); // solve
	int nRet = solver.Start(thisImpl, iSolver, nStage, Accuracy, NULL);
	thisImpl->End();
	return nRet;
}

int CRPSGSolverHelper::run_rpsg_verify(CRPSGSolverHelper* thisImpl)
{
	if(thisImpl == NULL) return -1;
	CPSGS_Problem* thisproblem = thisImpl->m_get_problem();
	thisproblem->m_clear_output_objects();
	thisproblem->m_set_problem_struct_type(UNKNOWN_PROBLEM_STRUCT_TYPE);
	thisImpl->m_set_whattodo(HELPER_REGIME_VERIFY_R); // solve
	CPSGSolver solver;
	thisImpl->Init();
	solver.SetInheritKey(0);
	if (thisproblem->m_get_name().empty()) {
		thisImpl->m_set_block_newoutputproblem(true);
		int res = solver.SInit(thisImpl);
		if (res != 0) {
			thisImpl->End();
			return 1;
		}
		int count = 0;
		while (++count <= 50) {
			if (!(thisproblem->m_get_name().empty())) {
				break;
			}
			sleep(100);
		}
		thisImpl->m_set_block_newoutputproblem(false);
		if (count>=50) {
			thisImpl->OnWarning(0, "PSG Solver", "Problem Name not defined");
			return 1;
		}
	}
	thisImpl->m_set_whattodo(HELPER_REGIME_VERIFY_R); // solve
	int nRet = solver.Verify(thisImpl);
	thisImpl->End();
	return nRet;
}
