#pragma once
#include <memory>
#include <stddef.h>

#ifndef MAX_PATH
#define MAX_PATH 260
#endif

class CPSGSolver
{
private:
	int m_inherit_key; 
public:
	enum sovler_status
	{
		solver_free,
		solver_running
	};
	CPSGSolver(void);
	~CPSGSolver(void);
	int SetInheritKey(int key) {int loc=m_inherit_key; m_inherit_key=key; return loc;}
	struct pack_parameter
	{
		CPSGSolver* pThis;
		void*		pUserData;
		int			TQSolver;
		int			NStage;
		double		Accuracy;
		char		szInitPointName[MAX_PATH];
	};
	int Start(void* pUserData, int TQSolver, int NStage, double Accuracy, const char* szInitPointName);
	int Verify(void* pUserData);
	int SInit(void* pUserData);
};

#ifdef WIN32
extern "C"
{
	CPSGSolver* CPSGSolver_new_empty(void);
	void CPSGSolver_delete(void* thisSolver);
	int CPSGSolver_Start(void* thisSolver, void* pUserData, int TQSolver, int NStage, double Accuracy, const char* szInitPointName);
	int CPSGSolver_Verify(void* thisSolver, void* pUserData);
	int CPSGSolver_SInit(void* thisSolver, void* pUserData);
}
#endif

