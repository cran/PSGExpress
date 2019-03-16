#include "PSGSolver.h"
#include "../../IPSGSolverHelper/src/IPSGSolverHelper.h"
#include <stdio.h>
#include <string.h>
#include <string>

#ifdef LINUX
extern "C"
{
	int vk_start(int TQSolver, int NStage, double Accuracy, const char* szInitPointName, void* pUserData);
	int vk_load_basis(long size, char* pBuff);
	int vk_save_basis();
	int vk_get_basis(long *size, char **pBuff);
	int vk_release_basis_buffer(char* pBuff);
	int vk_verify(void* pUserData);
	int vk_init(void* pUserData);
}
#else
extern "C"
{
	int vk_start(int TQSolver, int NStage, double Accuracy, const char* szInitPointName, void* pUserData);
	int vk_load_basis(long size, char* pBuff);
	int vk_save_basis();
	int vk_get_basis(long *size, char **pBuff);
	int vk_release_basis_buffer(char* pBuff);
	int vk_verify(void* pUserData);
	int vk_init(void* pUserData);
}
#endif

using namespace std;

CPSGSolver::CPSGSolver(void): m_inherit_key(0)
{}

CPSGSolver::~CPSGSolver(void)
{}

int CPSGSolver::SInit(void* pUserData)
{
	int nret = 0;
	IPSGSolverHelper* pI = static_cast<IPSGSolverHelper*>(pUserData);
	nret = vk_init(pI);
	return nret;
}

int CPSGSolver::Verify(void* pUserData)
{
	int nret = 0;
	IPSGSolverHelper* pI = static_cast<IPSGSolverHelper*>(pUserData);
	nret =  -1;
	if(pI->Init() == -1) {
		pI->SetTaskVerify(true);
		nret = vk_verify(pUserData);
		if (!nret) {
		}
		else {
			pI->OnProcessStop(nret);
		}
	}
	return nret;
}

int CPSGSolver::Start(void* pUserData, int TQSolver, int NStage, double Accuracy, const char* szInitPointName)
{
	int nret = 0;
	IPSGSolverHelper* pI = static_cast<IPSGSolverHelper*>(pUserData);
	nret =  -1;
	if(pI->Init() == -1) {
		pI->SetTaskVerify(false);
		nret = vk_start(TQSolver, NStage, Accuracy, NULL, pUserData);
		if (!nret) {
		}
		else {
			pI->OnProcessStop(nret);
		}
	}
	return nret;
}
