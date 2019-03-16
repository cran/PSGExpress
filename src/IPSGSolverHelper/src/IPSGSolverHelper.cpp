#include <vector>
#include <string>
#include <algorithm>
#ifdef WIN32
#include "windows.h"
#endif
#include <assert.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include "IPSGSolverHelper.h"
#include "exports_fn.h"

using namespace std;

#ifdef _MANAGED
#pragma managed(push, off)
#endif

#ifdef _MANAGED
#pragma managed(pop)
#endif

using namespace std;

extern "C"
{
	int vk_getfunctionvalue(const char* pBuffer, const char* pPointName, double* fValue, void* pUserData);
	int vk_getfunctiongradient(const char* pBuffer, const char* pPointName, double* fValue, void* pUserData);
	int vk_getfunctionincrement(const char* pBuffer, const char* pPointName, double* fValue, void* pUserData);
}

class CImpl
{
public:
	static volatile long m_nInstanceCount;
	bool m_bIsInit;
#ifdef WIN32
	CRITICAL_SECTION m_cs;
#elif LINUX
	pthread_mutex_t m_cs = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#else
#error Platform not supported
#endif
};

volatile long CImpl::m_nInstanceCount = 0;

IPSGSolverHelper::IPSGSolverHelper(void)
{
	m_pImpl = new CImpl;
	m_pImpl->m_bIsInit = false;
#ifdef WIN32
	InitializeCriticalSection(&(m_pImpl->m_cs));
#endif
}

IPSGSolverHelper::~IPSGSolverHelper(void)
{
#ifdef WIN32
	EnterCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
	pthread_mutex_lock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
	if(m_pImpl->m_nInstanceCount > 0 && !m_pImpl->m_bIsInit)
	{
#ifdef WIN32
		LeaveCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
		pthread_mutex_unlock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
		if(m_pImpl) {
			delete m_pImpl;
			m_pImpl = NULL;
		}
	}
#ifdef WIN32
	LeaveCriticalSection(&(m_pImpl->m_cs));
	DeleteCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
	pthread_mutex_unlock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
	if(m_pImpl) {
		delete m_pImpl;
		m_pImpl = NULL;
	}
}

bool IPSGSolverHelper::IsInit(void)
{
	return m_pImpl->m_bIsInit;
}

int IPSGSolverHelper::Init(void)
{
	if(m_pImpl->m_bIsInit)
		return -1;
#ifdef WIN32
	EnterCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
	pthread_mutex_lock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
	if(!m_pImpl->m_bIsInit)
	{
		m_pImpl->m_bIsInit = true;
		++(m_pImpl->m_nInstanceCount);
	}
	else
	{
#ifdef WIN32
		LeaveCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
		pthread_mutex_unlock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
	}
	int ret = m_pImpl->m_nInstanceCount;
#ifdef WIN32
	LeaveCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
	pthread_mutex_unlock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
	return ret;
}

void IPSGSolverHelper::End()
{
#ifdef WIN32
	EnterCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
	pthread_mutex_lock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
	if(!m_pImpl->m_bIsInit)
	{
		if(m_pImpl->m_nInstanceCount > 0)
		{
#ifdef WIN32
			LeaveCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
			pthread_mutex_unlock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
		}
	}
	else
	{
		--(m_pImpl->m_nInstanceCount);
		m_pImpl->m_bIsInit = false;
	}
#ifdef WIN32
	LeaveCriticalSection(&(m_pImpl->m_cs));
#elif LINUX
	pthread_mutex_unlock(&(m_pImpl->m_cs));
#else
#error Platform not supported
#endif
}

string toLower(const string& str)
{
	string t = str;
    transform(t.begin(), t.end(), t.begin(), ::tolower);
    return t;
}


extern "C"
{
int getexternalfunctioninfoex(const char* funcname, char*& pHeaderBuf, int *cols, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->GetExternalFunctionInfo(funcname, pHeaderBuf, cols);
	}
int getexternalfunctioninfodirex(const char* funcname, char*& pHeaderBuf, int* lenvars, int* lenscens, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->GetExternalFunctionInfoDir(funcname, pHeaderBuf, lenvars, lenscens);
	}

int runexternalfunctionex(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double* funvalue, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->RunExternalFunction(funcname, numvar, pHeaderBuf, varvalues, funvalue);
	}

int runexternalfunctiondirex(const char* funcname, char* pHeaderBuf, int lenvars, double* varvalues, int lenscens, int* numscens,
			 			 double*& probability, double*& funvalues, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->RunExternalFunctionDir(funcname, pHeaderBuf, lenvars, varvalues, lenscens, numscens,
	 			 probability, funvalues);
	}

int rungradientexternalfunctionex(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double** gradvalue, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->RunGradientExternalFunction(funcname, numvar, pHeaderBuf, varvalues, gradvalue);
	}

int rungradientexternalfunctiondirex(const char* funcname, char* pHeaderBuf, int lenvars, double* varvalues, int lenscens, int* numscen, double*& probability, double*& gradvalue, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->RunGradientExternalFunctionDir(funcname, pHeaderBuf, lenvars, varvalues, lenscens, numscen,
				probability, gradvalue);
	}

int saveismultyproblemex(int ismulty, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		if(pSolver->IsInit())
			return pSolver->SetMultyProblem(ismulty);
		else
		{
			pSolver->OnError(0xFFAA, "Set Problem Multy", "Object is not initialized");
			return -1;
		}
	}

int getproblemdescriptionex(char*& pBuffer, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		if(pSolver->IsInit())
			return pSolver->GetProblemDescription(pBuffer);
		else
		{
			pSolver->OnError(0xFFAA, "Problem Description", "Object is not initialized");
			return -1;
		}
	}

int  getpointex(const char* szPointName, char*& pBuffer, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->GetPoint(szPointName, pBuffer);
	}

void releasebufferex(void* pBuffer, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		pSolver->ReleaseBuffer(pBuffer);
	}

void releasematrixex(double* pMatrix, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		pSolver->ReleaseMatrix(pMatrix);
	}

bool saveobjectiveex(char szObjectiveName[max_objectname_len], double value, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveObjective(szObjectiveName, value);
	}

bool savevarsex(char szVarsName[][max_name_len], double values[], unsigned nCount, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveVars(szVarsName, values, nCount);
	}

bool savevectorex(const char* szVectorName, double varvalues[], unsigned nCount, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveVector(szVectorName, varvalues, nCount);
	}

bool savematrixex(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveMatrix(szMatrixName, szVarsName, matrixdata, numcol, numrow);
	}

bool savepointex(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SavePoint(szPointName, szVarsName, values, nCount);
	}

bool addpointex(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->AddPoint(szPointName, szVarsName, values, nCount);
	}

bool addvectorex(const char* szVectorName, double varvalues[], unsigned nCount, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->AddVector(szVectorName, varvalues, nCount);
	}

bool addmatrixex(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->AddMatrix(szMatrixName, szVarsName, matrixdata, numcol, numrow);
	}


bool saveobjsex(char szObjName[][max_objectname_len],
				double val[],
				char szConstrName[][max_name_len],
				char type[],
				unsigned nCount,
				void* pUserData
				)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveObjs(szObjName, val, szConstrName, type, nCount);
	}

bool saveconstraintsex(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveConstraints(szConstrName, val, type, nCount);
	}

bool saveconstraintsslackex(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveConstraintsSlack(szConstrName, val, type, nCount);
	}

bool savestatusex(const char* szStatus, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveStatus(szStatus);
	}

bool getmatrixdataex(const char* lpszMatrixName, double*& pData,  int rows, int cols, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->GetMatrixData(lpszMatrixName, pData,  rows, cols);
	}

bool getmatrixdataspex(const char* lpszMatrixName, double*& pData,  int*& prows, int*& pcols, int coeffs, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->GetMatrixDataSp(lpszMatrixName, pData,  prows, pcols, coeffs);
	}

int  getmatrixinfoex(const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->GetMatrixInfo(lpszMatrixName, pHeaderBuf, rows, cols);
	}

int  getmatrixinfospex(const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols, int* coeffs, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->GetMatrixInfoSp(lpszMatrixName, pHeaderBuf, rows, cols, coeffs);
	}

void onmessageex(int nMsgCode, char* szMessage, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		pSolver->OnMessage(nMsgCode, szMessage);
	}

void onerrorex(int nErrCode, const char* lpszObject, const char* lpszDescription, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		pSolver->OnError(nErrCode, lpszObject, lpszDescription);
	}

void onwarningex(int nWarnCode, const char* lpszObject, const char* lpszDescription, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		pSolver->OnWarning(nWarnCode, lpszObject, lpszDescription);
	}

int onfinishex(void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->OnFinish();
	}

int oncancelex( void* pUserData )
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->OnCancel();
	}

void getrootpathex(char* pBuffer, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		pSolver->GetRootPath(pBuffer);
	}

int getlogparamex(char* pBuffer, void* pUserData)
	{
		long logparam;
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		logparam = pSolver->GetLogParam(pBuffer);
		return logparam;
	}

int saveproblemnameex(char* p_status, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveProblemName(p_status);
	}

int oncyclefinishex(void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->OnCycleFinish();
	}

int saveproblemstatementex(char* p_status, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveProblemStatement(p_status);
	}

int savesolutionex(char* p_solution, void* pUserData)
	{
		assert(pUserData != NULL);
		IPSGSolverHelper* pSolver = static_cast<IPSGSolverHelper*>(pUserData);
		return pSolver->SaveSolution(p_solution);
	}
}
