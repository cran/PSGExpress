extern "C"
{
	int getexternalfunctioninfoex(const char* funcname, char*& pHeaderBuf, int *cols, void* pUserData);
	int runexternalfunctionex(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double* funvalue, void* pUserData);
	int rungradientexternalfunctionex(const char* funcname, int numvar, char* pHeaderBuf, double* varvalues, double** gradvalue, void* pUserData);

//	int GetFunctionValueex(const char* pBuffer, const char* pPointName, double* fvalue, void* pUserData);
//	int GetFunctionGradientex(const char* pBuffer, const char* pPointName, void* pUserData);

	int getproblemdescriptionex(char*& pBuffer, void* pUserData);
	int  getpointex(const char* szPointName, char*& pBuffer, void* pUserData);
	bool getmatrixdataex(const char* lpszMatrixName, double*& pData,  int rows, int cols, void* pUserData);
	bool getmatrixdataspex(const char* lpszMatrixName, double*& pData,  int*& prows, int*& pcols, int coeffs, void* pUserData);
	int  getmatrixinfoex(const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols, void* pUserData);
	int  getmatrixinfospex(const char* lpszMatrixName, char*& pHeaderBuf, int* rows, int* cols, int* coeffs, void* pUserData);

	void releasebufferex(void* pBuffer, void* pUserData);
	void releasematrixex(double* pMatrix, void* pUserData);

	bool saveobjectiveex(char szObjectiveName[max_objectname_len], double value, void* pUserData);
	bool savevarsex(char szVarsName[][max_name_len], double values[], unsigned nCount, void* pUserData);
	bool savevectorex(const char* szVectorName, double varvalues[], unsigned nCount, void* pUserData);
	bool savematrixex(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow, void* pUserData);
	bool savepointex(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount, void* pUserData);
	bool saveobjsex(
									char szObjName[][max_objectname_len],
									double val[], 
									char szConstrName[][max_name_len],
									char type[],
									unsigned nCount, 
									void* pUserData
							   );
	bool saveconstraintsex(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount, void* pUserData);
	bool saveconstraintsslackex(char szConstrName[][max_name_len], double val[], char type[], unsigned nCount, void* pUserData);
	bool savestatusex(const char* szStatus, void* pUserData);
	int saveismultyproblemex(int ismulty, void* pUserData);

//	void adderrorex(const char* lpszObject, const char* lpszDescription, void* pUserData);
//	void addwarningex(const char* lpszObject, const char* lpszDescription, void* pUserData);

	void onmessageex(int nMsgCode, char* szMessage, void* pUserData);
	void onerrorex(int nErrCode, const char* lpszObject, const char* lpszDescription, void* pUserData);
	void onwarningex(int nWarnCode, const char* lpszObject, const char* lpszDescription, void* pUserData);
	int  oncancelex(void* pUserData);
	int  onfinishex(void* pUserData);
	void getrootpathex(char* pBuffer, void* pUserData);
	int getlogparamex(char* pBuffer, void* pUserData);
	int saveproblemnameex(char* p_status, void* pUserData);
	int oncyclefinishex(void* pUserData);
	int saveproblemstatementex(char* p_status, void* pUserData);
	int savesolutionex(char* p_solution, void* pUserData);

	bool addpointex(const char* szPointName, char szVarsName[][max_name_len], double values[], unsigned nCount, void* pUserData);
	bool addvectorex(const char* szVectorName, double varvalues[], unsigned nCount, void* pUserData);
	bool addmatrixex(const char* szMatrixName, char szVarsName[][max_name_len], double matrixdata[], int numcol, int numrow, void* pUserData);

// Gurobi
	int isgrbinstalledex(void* pUserData);
	int istheregrblicenseex(void* pUserData);
	int initpsggrbinterfaceex(const char* logfilename, void* pUserData);
	int freepsggrbenvironmentex(void* pUserData);
	int freepsggrbInterfaceex(void* pUserData);
	int addgrbmodelcroldex(const char* modelname, void* pUserData);
	int addgrbmodelcrex(const char* modelname, void* pUserData);
	int addgrbmodelldex(const char* modelname, int numvars,
		int numconstrs, int objsense, double objcon, double *obj, char *sense, double *rhs, int *vbeg,
		int *vlen, int *vind, double *vval, double *lb, double *ub, char *vtype, char **varnames,
		char **constrnames, void* pUserData);
	int addvarstogrbmodelex(const char* modelname, int numvars,
		int numnz, int *vbeg, int *vind, double *vval, double *obj, double *lb, double *ub,
		char *vtype, const char **varnames, void* pUserData);
	int releasegrbmodelex(const char* modelname, void* pUserData);
	int addconstrainttogrbmodelex(const char* modelname, int numnz, int *cind, double *cval, char sense, double rhs, char *constrname, void* pUserData);
	int addconstraintstogrbmodelex(const char* modelname, int numconstrs, int numnz, int *cbeg, int *cind,  double *cval, char *sense, double *rhs, char **constrnames, void* pUserData);
	int addqconstrainttogrbmodelex(const char* modelname, int numlnz, int *lind, double *lval, int numqnz, int *qrow, int *qcol, double *qval, char sense, double rhs, char *constrname, void* pUserData);
	int removeconstraintfromgrbmodelex(const char* modelname, int numdel, int *ind, void* pUserData);
	int addqptermstogrbmodelex(const char* modelname, int numqnz, int *qrow, int *qcol, double *qval, void* pUserData);
	int getdblattrfromgrbmodelex(const char* modelname, char *attrname, double *valueP, void* pUserData);
	int getdblattrarrayfromgrbmodelex(const char* modelname, char *attrname, int first, int len, double *values, void* pUserData);
	int getintattrfromgrbmodelex(const char* modelname, char *attrname, int *valueP, void* pUserData);
	int getintattrarrayfromgrbmodelex(const char* modelname, char *attrname, int first, int len, int *values, void* pUserData);
	int getdblparamtogrbenvex(char *paramname, double *value, void* pUserData);
	int setdblattrfromgrbmodelex(const char* modelname, char *attrname, double newvalue, void* pUserData);
	int setdblattrarraytogrbmodelex(const char* modelname, char *attrname, int first, int len, double *newvalues, void* pUserData);
	int setintattrtogrbmodelex(const char* modelname, char *attrname, int newvalue, void* pUserData);
	int setintattrarraytogrbmodelex(const char* modelname, char *attrname, int first, int len, int *newvalues, void* pUserData);
	int setdblparamtogrbmodelex(const char* modelname, char *paramname, double newvalue, void* pUserData);
	int setintparamtogrbmodelex(const char* modelname, char *paramname, int newvalue, void* pUserData);
	int getintparamfromgrbmodelex(const char* modelname, char *paramname, int *valueP, void* pUserData);
	int getdblparamfromgrbmodelex(const char* modelname, char *paramname, double *valueP, void* pUserData);

	int setdblparamtogrbenvex(char *paramname, double value, void* pUserData);
	int setintparamtogrbenvex(char *paramname, int value, void* pUserData);
	int optimizegrbmodelex(const char* modelname, void* pUserData);
	int terminategrbmodelex(const char* modelname, void* pUserData);
	int tunegrbmodelex(const char* modelname, void* pUserData);
	int updategrbmodelex(const char* modelname, void* pUserData);
	int resetgrbmodelbasetimeex(const char* modelname, void* pUserData);
	int readgrbmodeloldex(const char* filename, const char* modelname, void* pUserData);
	int writegrbmodeloldex(const char* modelname, const char* filename, void* pUserData);
	int getgurobierrorex(char* buffer, int bufferlen, void* pUserData);
}
