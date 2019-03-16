#include "../../PSGShellObjects/src/PSGShellObjects.h"
#include "../../RPSGSolverHelper/src/RPSGSolverHelper.h"
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>

#ifdef ERROR
#undef ERROR
#endif

#include "RPSG_Functions.h"
#include <R.h>
#include <Rinternals.h>

using namespace std;

enum RPSG_REGIME_TYPE {
		RPSG_REGIME_UNKNOWN = 1,
		RPSG_REGIME_SOLVE,
		RPSG_REGIME_VERIFY,
		RPSG_REGIME_EXPORTTOTEXT,
		RPSG_REGIME_IMPORTFROMTEXT,
		RPSG_REGIME_GETFUNCVALUE,
		RPSG_REGIME_GETFUNCINCR,
		RPSG_REGIME_GETFUNCSENS
};

extern "C"  void PrintToR(const char* strmess, WHATPRINT_TYPE_R what);
extern "C"  void ProcessStopToR(const char* strmess, int param);
extern "C"  void CycleFinishToR(const char* strmess, int param);
extern "C"  void ProcessCancelToR(void);
extern "C"  int RunRExternalFunction(EXT_FUNCTION_DESCR_R* fundescr,  int numvar, const char* header, double* varvalues_arr, CRPSGSolverHelper* thisImpl, double* funval);
extern "C"  int RunRGradientExternalFunction(EXT_FUNCTION_DESCR_R* fundescr, int numvar, const char* header, double* varvalues_arr, CRPSGSolverHelper* thisImpl, double** gradvalues_arr);
extern "C"  int RunRExternalFunctionDir(EXT_FUNCTION_DESCR_R* fundescr, const char* pHeaderBuf, int lenvars, double* varvalues, int lenscens, int* numscens, CRPSGSolverHelper* thisImpl,
		 double*& probability, double*& funvalues);
extern "C"  int RunRGradientExternalFunctionDir(EXT_FUNCTION_DESCR_R* fundescr, const char* pHeaderBuf,
		int lenvars, double* varvalues, int lenscens, int* numscen, CRPSGSolverHelper* thisImpl, double*& probability, double*& gradvalue);
extern "C"  SEXP  crpsg_importfromtext(SEXP rpathtostatement, SEXP suppressMessages);
extern "C"  SEXP  crpsg_getfunctionvalue(SEXP regime, SEXP thisfunclist, SEXP rho, SEXP allowExt, SEXP suppressMessages, SEXP allowFunVal);
extern "C"  SEXP  crpsg_solver(SEXP regime, SEXP path, SEXP thisproblem, SEXP rho, SEXP allowExt, SEXP suppressMessages);

string toLower(const string& str); 

SEXP construct_string_array(vector<string>* thisstr_arr, string& errorstr);
SEXP construct_string(string thisstr, string& errorstr);
SEXP construct_logical(bool key, string& errorstr);
SEXP construct_real(double* val, int num, string& errorstr);
SEXP construct_integer(int* val, int num, string& errorstr);
SEXP construct_matrix(CPSGS_Data_Object* this_object, string& errorstr);
SEXP construct_pmatrix(CPSGS_Data_Object* this_object, string& errorstr);
SEXP construct_point(CPSGS_Data_Object* this_object, string& errorstr);
SEXP construct_vector(CPSGS_Data_Object* this_object, string& errorstr);
SEXP construct_PSG_Object(CPSGS_Data_Object* this_object, string& errorstr);
SEXP construct_PSG_Problem_Output(CPSGS_OutputProblem* this_out_problem, vector<string>& error_str_arr, bool& res);
SEXP construct_PSG_Problem_Direct(CPSGS_Problem* this_problem, vector<string>& error_str_arr, bool& res);
SEXP lookforExternelObject(string objectname, SEXP rho);
SEXP lookforExternelObjectExt(string objectname, string statement, SEXP rho, string& errorstr);
SEXP decode_sparse(SEXP input_obj, SEXP rho, int& nrow, int& ncol, vector<string>& colnames, vector<int>& ivec, 
	vector<int>& jvec, vector<double>& xvec, string& errorstr);
bool encode_rvstring_tostring(SEXP thisrvstring, string& outstr, string& errorstr);
bool encode_RPSG_Problem(SEXP thisproblem, CPSGS_Problem* this_problem, SEXP rho, vector<string>& error_str_arr);
bool encode_RPSG_Function(SEXP thisfunclist, CPSGS_Problem* this_problem, SEXP rho, vector<string>& error_str_arr);
bool check_and_correct_pathimportexport(string& path_toexport, bool key_import, string& thisproblem_name, string& errorstr);
bool construct_PSG_POINT_from_robject(SEXP thisRObject, string objname,  CPSGS_Data_Object** thisPSGPoint, string& errorstr);
bool check_ext_functions(vector<EXT_FUNCTION_DESCR_R>* extfunction_arr, SEXP rho, CRPSGSolverHelper* thisImpl, vector<string>& errors_arr);
int split_text_to_vector(const char* inpstr, vector<string>& out_str_arr);
int split_text_to_vector_light(const char* inpstr, vector<string>& out_str_arr);
int find_original_object_name(string objectname, string statement, vector<string>& original_names);
namespace strutil_R {
char *strlwr(char *str)
{
  unsigned char *p = (unsigned char *)str;
  while (*p) {
     *p = tolower((unsigned char)*p);
      p++;
  }
  return str;
}
}

 void PrintToR(const char* strmess, WHATPRINT_TYPE_R what)
{
	if (strmess!=NULL) Rprintf("%s\n", strmess);
}

 void ProcessStopToR(const char* strmess, int param)
{
	if (strmess!=NULL) Rprintf("%s\n", strmess);
}

 void CycleFinishToR(const char* strmess, int param)
{
	if (strmess!=NULL) Rprintf("%s\n", strmess);
}

 void ProcessCancelToR(void)
{
	return;
}

int RunRExternalFunctionDir(EXT_FUNCTION_DESCR_R* thisdescr, const char* pHeaderBuf, int lenvars, double* varvalues_arr, int lenscens, int* numscens, CRPSGSolverHelper* thisImpl,
 		 double*& probability, double*& funvalues)
 {
	SEXP rho = (SEXP)thisdescr->fnenv;
	if (!isEnvironment(rho)) {
		char buffererr[128];
		sprintf(buffererr, "Environment for %s not found", (thisdescr->fnname).c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP thisfun = PROTECT(findFun( install(thisdescr->fnname.c_str()), rho));
	if (thisfun == R_NilValue) {
		UNPROTECT(1);
		char buffererr[128];
		sprintf(buffererr, "%s not found", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	string errorstr;
	int numvars = lenvars;
	SEXP point_arg = PROTECT(construct_real(varvalues_arr, numvars, errorstr));
	if (point_arg == R_NilValue) {
		UNPROTECT(3);
		char buffererr[128];
		sprintf(buffererr, "Cannot construct point for %s", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	vector<string> this_varnames_arr;
	int numvarsloc = split_text_to_vector_light(pHeaderBuf, this_varnames_arr);
	if (numvarsloc != numvars) {
		UNPROTECT(3);
		char buffererr[128];
		sprintf(buffererr, "Different numbers of variables in %s", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP r_header = PROTECT(allocVector(STRSXP, numvars));
	for (int64_t i=0; i<numvars; i++) {
		SET_STRING_ELT(r_header, i, mkChar(this_varnames_arr[i].c_str()));
	}
	setAttrib(point_arg, R_NamesSymbol, r_header);
	SEXP scens_arg = PROTECT(construct_integer(numscens, lenscens, errorstr));
	int numarg = (int)thisdescr->str_fn_args.size();
	if (numarg>4) {
		UNPROTECT(5);
		char buffererr[128];
		sprintf(buffererr, "Too many arguments in %s. Four is maximum value.", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP RCallBack;
	PROTECT(RCallBack = allocVector(LANGSXP, numarg+1)); // RFun + arguments
	int num_unprotect = 5;
	SETCAR(RCallBack, thisfun);
	SETCADR(RCallBack, point_arg);
	SETCADDR(RCallBack, scens_arg);
	if (numarg>2) {
		SEXP thisarg1 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[2], rho));
		num_unprotect++;
		if (thisarg1== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[2].c_str(), thisdescr->fnname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCADDDR(RCallBack, thisarg1);
	}
	if (numarg>3) {
		SEXP thisarg2 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[3].c_str(), rho));
		num_unprotect++;
		if (thisarg2== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[3].c_str(), thisdescr->fnname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCAD4R(RCallBack, thisarg2);
	}
	SEXP rout;
	rout = PROTECT(eval(RCallBack, rho));
	num_unprotect++;
	if (rout == R_NilValue) {
		UNPROTECT(num_unprotect);
		char buffererr[128];
		sprintf(buffererr, "%s returned NULL value", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	double* doubleout = REAL(rout);
	for (int i=0; i<numvars; i++) {
		funvalues[i] = doubleout[i];
	}
	UNPROTECT(num_unprotect);
	return 1;
 }

int RunRGradientExternalFunctionDir(EXT_FUNCTION_DESCR_R* thisdescr, const char* pHeaderBuf,
		int lenvars, double* varvalues, int lenscens, int* numscen, CRPSGSolverHelper* thisImpl, double*& probability, double*& gradvalue)
 {
	SEXP rho = (SEXP)thisdescr->fnenv;
	if (!isEnvironment(rho)) {
		char buffererr[128];
		sprintf(buffererr, "Environment for %s not found", (thisdescr->grname).c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP thisfun = PROTECT(findFun( install(thisdescr->grname.c_str()), rho));
	if (thisfun == R_NilValue) {
		UNPROTECT(1);
		char buffererr[128];
		sprintf(buffererr, "%s not found", thisdescr->grname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	string errorstr;
	int numvars = lenvars;
	SEXP point_arg = PROTECT(construct_real(varvalues, numvars, errorstr));
	if (point_arg == R_NilValue) {
		UNPROTECT(3);
		char buffererr[128];
		sprintf(buffererr, "Cannot construct point for %s", thisdescr->grname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	vector<string> this_varnames_arr;
	int numvarsloc = split_text_to_vector_light(pHeaderBuf, this_varnames_arr);
	if (numvarsloc != numvars) {
		UNPROTECT(3);
		char buffererr[128];
		sprintf(buffererr, "Different numbers of variables in %s", thisdescr->grname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP r_header = PROTECT(allocVector(STRSXP, numvars));
	for (int64_t i=0; i<numvars; i++) {
		SET_STRING_ELT(r_header, i, mkChar(this_varnames_arr[i].c_str()));
	}
	setAttrib(point_arg, R_NamesSymbol, r_header);
	SEXP scens_arg = PROTECT(construct_integer(numscen, lenscens, errorstr));
	int numarg = (int)thisdescr->str_fn_args.size();
	if (numarg>4) {
		UNPROTECT(5);
		char buffererr[128];
		sprintf(buffererr, "Too many arguments in %s. Four is maximum value.", thisdescr->grname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP RCallBack;
	PROTECT(RCallBack = allocVector(LANGSXP, numarg+1)); // RFun + arguments
	int num_unprotect = 5;
	SETCAR(RCallBack, thisfun);
	SETCADR(RCallBack, point_arg);
	SETCADDR(RCallBack, scens_arg);
	if (numarg>2) {
		SEXP thisarg1 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[2], rho));
		num_unprotect++;
		if (thisarg1== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[2].c_str(), thisdescr->grname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCADDDR(RCallBack, thisarg1);
	}
	if (numarg>3) {
		SEXP thisarg2 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[3].c_str(), rho));
		num_unprotect++;
		if (thisarg2== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[3].c_str(), thisdescr->grname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCAD4R(RCallBack, thisarg2);
	}
	SEXP rout;
	rout = PROTECT(eval(RCallBack, rho));
	num_unprotect++;
	if (rout == R_NilValue) {
		UNPROTECT(num_unprotect);
		char buffererr[128];
		sprintf(buffererr, "%s returned NULL value", thisdescr->grname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	double* doubleout = REAL(rout);
	for (int64_t i=0; i<lenscens; i++) {
		for (int64_t j=0; j<numvars; j++) {
			*(gradvalue+i*numvars+j) = *(doubleout+j*lenscens+i);
		}
	}
	UNPROTECT(num_unprotect);
	return 1;
 }

 int RunRExternalFunction(EXT_FUNCTION_DESCR_R* thisdescr, int numvar, const char* header, double* varvalues_arr, CRPSGSolverHelper* thisImpl, double* funval)
{
	SEXP rho = (SEXP)thisdescr->fnenv;
	if (!isEnvironment(rho)) {
		char buffererr[128];
		sprintf(buffererr, "Environment for %s not found", (thisdescr->fnname).c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP thisfun = PROTECT(findFun( install(thisdescr->fnname.c_str()), rho));
	if (thisfun == R_NilValue) {
		UNPROTECT(1);
		char buffererr[128];
		sprintf(buffererr, "%s not found", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	string errorstr;
	int numvars = (int)thisdescr->str_fn_vars.size();
	SEXP point_arg = PROTECT(construct_real(varvalues_arr, numvars, errorstr));
	if (point_arg == R_NilValue) {
		UNPROTECT(3);
		char buffererr[128];
		sprintf(buffererr, "Cannot construct point for %s", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP r_header = PROTECT(allocVector(STRSXP, numvars));
	for (int64_t i=0; i<numvars; i++) {
		SET_STRING_ELT(r_header, i, mkChar(thisdescr->str_fn_vars[i].c_str()));
	}
	setAttrib(point_arg, R_NamesSymbol, r_header);
	int numarg = (int)thisdescr->str_fn_args.size();
	SEXP RCallBack;
	PROTECT(RCallBack = allocVector(LANGSXP, numarg+1));
	int num_unprotect = 4;
	SETCAR(RCallBack, thisfun);
	SETCADR(RCallBack, point_arg);
	if (numarg>1) {
		SEXP thisarg1 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[1], rho));
		num_unprotect++;
		if (thisarg1== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[1].c_str(), thisdescr->fnname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCADDR(RCallBack, thisarg1);
	}
	if (numarg>2) {
		SEXP thisarg2 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[2].c_str(), rho));
		num_unprotect++;
		if (thisarg2== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[2].c_str(), thisdescr->fnname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCADDDR(RCallBack, thisarg2);
	}
	if (numarg>3) {
		SEXP thisarg3 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[3], rho));
		num_unprotect++;
		if (thisarg3== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", (thisdescr->str_fn_args[3]).c_str(), thisdescr->fnname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCAD4R(RCallBack, thisarg3);
	}
	SEXP rout;
	rout = PROTECT(eval(RCallBack, rho));
	num_unprotect++;
	if (rout == R_NilValue) {
		UNPROTECT(num_unprotect);
		char buffererr[128];
		sprintf(buffererr, "%s returned NULL value", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	*funval = *(REAL(rout));
	UNPROTECT(num_unprotect);
	return 1;
}

int RunRGradientExternalFunction(EXT_FUNCTION_DESCR_R* thisdescr, int numvar, const char* header, double* varvalues_arr, CRPSGSolverHelper* thisImpl, double** gradvalues_arr)
{
	SEXP rho = (SEXP)thisdescr->fnenv;
	if (!isEnvironment(rho)) {
		char buffererr[128];
		sprintf(buffererr, "Environment for %s not found", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP thisgrad = PROTECT(findFun( install(thisdescr->grname.c_str()), rho));
	if (thisgrad == R_NilValue) {
		UNPROTECT(2);
		char buffererr[128];
		sprintf(buffererr, "%s not found", thisdescr->grname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	string errorstr;
	int numvars = (int)thisdescr->str_fn_vars.size();
	SEXP point_arg = PROTECT(construct_real(varvalues_arr, numvars, errorstr));
	if (point_arg == R_NilValue) {
		UNPROTECT(3);
		char buffererr[128];
		sprintf(buffererr, "Cannot construct point for %s", thisdescr->grname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	SEXP r_header = PROTECT(allocVector(STRSXP, numvars));
	for (int64_t i=0; i<numvars; i++) {
		SET_STRING_ELT(r_header, i, mkChar(thisdescr->str_fn_vars[i].c_str()));
	}
	setAttrib(point_arg, R_NamesSymbol, r_header);
	int numarg = (int)thisdescr->str_fn_args.size();
	SEXP RCallBack;
	PROTECT(RCallBack = allocVector(LANGSXP, numarg+1)); // RFun + arguments
	int num_unprotect = 4;
	SETCAR(RCallBack, thisgrad);
	SETCADR(RCallBack, point_arg);
	if (numarg>1) {
		SEXP thisarg1 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[1], rho));
		num_unprotect++;
		if (thisarg1== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[1].c_str(), thisdescr->grname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCADDR(RCallBack, thisarg1);
	}
	if (numarg>2) {
		SEXP thisarg2 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[2], rho));
		num_unprotect++;
		if (thisarg2== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[2].c_str(), thisdescr->grname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCADDDR(RCallBack, thisarg2);
	}
	if (numarg>3) {
		SEXP thisarg3 = PROTECT(lookforExternelObject(thisdescr->str_fn_args[3], rho));
		num_unprotect++;
		if (thisarg3== R_UnboundValue) {
			UNPROTECT(num_unprotect);
			char buffererr[128];
			sprintf(buffererr, "Cannot find \"%s\" argument for %s", thisdescr->str_fn_args[3].c_str(), thisdescr->grname.c_str());
			thisImpl->OnError(1, "External Function", buffererr);
			return 0;
		}
		SETCAD4R(RCallBack, thisarg3);
	}
	SEXP rout;
	rout = PROTECT(eval(RCallBack, rho));
	num_unprotect++;
	if (rout == R_NilValue) {
		UNPROTECT(num_unprotect);
		char buffererr[128];
		sprintf(buffererr, "%s returned NULL value", thisdescr->fnname.c_str());
		thisImpl->OnError(1, "External Function", buffererr);
		return 0;
	}
	double* doubleout = REAL(rout);
	for (int i=0; i<numvars; i++) {
		(*gradvalues_arr)[i] = doubleout[i];
	}
	UNPROTECT(num_unprotect);
	return 1;
}

SEXP construct_string_array(vector<string>* thisstr_arr, string& errorstr)
{
	errorstr.clear();
	if (thisstr_arr->empty()) {
		char buffererr[128];
		sprintf(buffererr, "String Array is Empty");
		errorstr = string(buffererr);
		return R_NilValue;
	}
	SEXP outstr = PROTECT(allocVector(STRSXP, thisstr_arr->size()));
	for (size_t i=0; i<thisstr_arr->size(); i++) {
		SET_STRING_ELT(outstr, i, mkChar(thisstr_arr->at(i).c_str()));
	}
	UNPROTECT(1);
	return outstr;
}

SEXP construct_string(string thisstr, string& errorstr)
{
	errorstr.clear();
	SEXP outstr = PROTECT(allocVector(STRSXP, 1));
	SET_STRING_ELT(outstr, 0, mkChar(thisstr.c_str()));
	UNPROTECT(1);
	return outstr;
}

SEXP construct_logical(bool key, string& errorstr)
{
	errorstr.clear();
	SEXP outstr = PROTECT(allocVector(LGLSXP, 1));
	LOGICAL(outstr)[0] = key?true:false;
	UNPROTECT(1);
	return outstr;
}

SEXP construct_integer(int* val, int num, string& errorstr)
{
	errorstr.clear();
	SEXP outstr = PROTECT(allocVector(INTSXP, num));
	for (int i=0; i<num; i++) {
		INTEGER(outstr)[i] = val[i];
	}
	UNPROTECT(1);
	return outstr;
}

SEXP construct_real(double* val, int num, string& errorstr)
{
	errorstr.clear();
	SEXP outstr = PROTECT(allocVector(REALSXP, num));
	for (int i=0; i<num; i++) {
		REAL(outstr)[i] = val[i];
	}
	UNPROTECT(1);
	return outstr;
}

SEXP construct_pmatrix(CPSGS_Data_Object* this_object, string& errorstr)
{
	errorstr.clear();
	if (this_object->m_get_type() != PSGS_PMATRIX) {
		char buffererr[128];
		sprintf(buffererr, "%s is not a PSG PMatrix", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	SEXP abstrclass, thisclassimpl;
	abstrclass = PROTECT(allocVector(STRSXP, 1));
	if (abstrclass == R_NilValue) {
		char buffererr[128];
		sprintf(buffererr, "%s: Cannot create R Object", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	thisclassimpl = PROTECT(allocS4Object());
	SET_S4_OBJECT(thisclassimpl);
	if (thisclassimpl == R_NilValue) {
		UNPROTECT(1);
		char buffererr[128];
		sprintf(buffererr, "%s: Cannot create S4 Object", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	SET_STRING_ELT(abstrclass, 0, mkChar("dgTMatrix"));
	classgets(thisclassimpl, abstrclass);
	if (thisclassimpl == R_NilValue) {
		UNPROTECT(1);
		char buffererr[128];
		sprintf(buffererr, "%s: dgTMatrix Class not found", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	int64_t nrow, ncol, nnz;
	vector<int64_t> size_arr = this_object->m_get_sizes();
	nrow = size_arr[0]; ncol = size_arr[1]; nnz = this_object->m_get_nnz();
	vector<string>* thisheader_arr = this_object->m_get_headrarray();
	double* thisdata_arr = this_object->m_get_data();
	if ((nrow*ncol)==0 || thisheader_arr->empty() || (thisdata_arr==NULL)) {
		UNPROTECT(1);
		char buffererr[128];
		sprintf(buffererr, "%s is empty", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	SEXP r_factors = PROTECT(allocVector(VECSXP, 1));
	SEXP r_thisfactors = PROTECT(allocList(0));
	SET_VECTOR_ELT(r_factors, 0, r_thisfactors);
	SEXP r_dims = PROTECT(allocVector(INTSXP, 2));
	INTEGER(r_dims)[0] = nrow;
	INTEGER(r_dims)[1] = ncol;
	SEXP r_thisi = PROTECT(allocVector(INTSXP, nnz));
	int* pr_thisi = INTEGER(r_thisi);
	SEXP r_thisj = PROTECT(allocVector(INTSXP, nnz));
	int* pr_thisj = INTEGER(r_thisj);
	SEXP r_thisx = PROTECT(allocVector(REALSXP, nnz));
	double* pr_thisx = REAL(r_thisx);
	double* p_data = thisdata_arr;
	for (int64_t i=0; i<nnz; i++) {
		pr_thisi[i] = (int)(*p_data++);
		pr_thisj[i] = (int)(*p_data++);
		pr_thisx[i] = (*p_data++);
	}
	setAttrib(thisclassimpl, install("i"), r_thisi);
	setAttrib(thisclassimpl, install("j"), r_thisj);
	setAttrib(thisclassimpl, install("x"), r_thisx);
	setAttrib(thisclassimpl, install("Dim"), r_dims);
	setAttrib(thisclassimpl, install("factors"), r_factors);
	SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, R_NilValue);
	SEXP r_header = PROTECT(allocVector(STRSXP, ncol));
	for (int64_t j=0; j<ncol; j++) {
		SET_STRING_ELT(r_header, j, mkChar(thisheader_arr->at(j).c_str()));
	}
	SET_VECTOR_ELT(dimnames, 1, r_header);
	setAttrib(thisclassimpl, install("Dimnames"), dimnames);
	UNPROTECT(10);
	return thisclassimpl;
}

SEXP construct_matrix(CPSGS_Data_Object* this_object, string& errorstr)
{
	errorstr.clear();
	if (this_object->m_get_type() != PSGS_MATRIX) {
		char buffererr[128];
		sprintf(buffererr, "%s is not a PSG Matrix", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	int64_t nrow, ncol;
	vector<int64_t> size_arr = this_object->m_get_sizes();
	nrow = size_arr[0]; ncol = size_arr[1];
	if (size_arr[1] > 14) return R_NilValue;
	vector<string>* thisheader_arr = this_object->m_get_headrarray();
	double* thisdata_arr = this_object->m_get_data();
	if ((nrow*ncol)==0 || thisheader_arr->empty() || (thisdata_arr==NULL)) {
		char buffererr[128];
		sprintf(buffererr, "%s is empty", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	SEXP r_thismatrix = PROTECT(allocMatrix(REALSXP, nrow, ncol));
	double* pr_thismatrix = REAL(r_thismatrix);
	for (int64_t i=0; i<nrow; i++) {
		for (int64_t j=0; j<ncol; j++) {
			pr_thismatrix[i+j*nrow] = thisdata_arr[j+i*ncol];
		}
	}
	if (ncol > 21) return R_NilValue;
	SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, R_NilValue);
	SEXP r_header = PROTECT(allocVector(STRSXP, ncol));
	for (int64_t j=0; j<ncol; j++) {
		SET_STRING_ELT(r_header, j, mkChar(thisheader_arr->at(j).c_str()));
	}
	SET_VECTOR_ELT(dimnames, 1, r_header);
	setAttrib(r_thismatrix, R_DimNamesSymbol, dimnames);
	UNPROTECT(3);
	return r_thismatrix;
}

SEXP construct_point(CPSGS_Data_Object* this_object, string& errorstr)
{
	errorstr.clear();
	if (this_object->m_get_type() != PSGS_POINT) {
		char buffererr[128];
		sprintf(buffererr, "%s is not a PSG Point", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	int64_t nelem;
	vector<int64_t> size_arr = this_object->m_get_sizes();
	nelem = size_arr[0];
	vector<string>* thisheader_arr = this_object->m_get_headrarray();
	double* thisdata_arr = this_object->m_get_data();
	if (nelem==0 || thisheader_arr->empty() || (thisdata_arr==NULL)) {
		char buffererr[128];
		sprintf(buffererr, "%s is empty", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	SEXP r_thispoint = PROTECT(allocVector(REALSXP, nelem));
	double* pr_thispoint = REAL(r_thispoint);
	for (int64_t i=0; i<nelem; i++) {
		pr_thispoint[i] = thisdata_arr[i];
	}
	if ((int)nelem > 21) return R_NilValue;
	SEXP r_header = PROTECT(allocVector(STRSXP, nelem));
	for (int64_t i=0; i<nelem; i++) {
		SET_STRING_ELT(r_header, i, mkChar(thisheader_arr->at(i).c_str()));
	}
	setAttrib(r_thispoint, R_NamesSymbol, r_header);
	UNPROTECT(2);
	return r_thispoint;
}

SEXP construct_vector(CPSGS_Data_Object* this_object, string& errorstr)
{
	errorstr.clear();
	if (this_object->m_get_type() != PSGS_VECTOR) {
		char buffererr[128];
		sprintf(buffererr, "%s is not a PSG Vector", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	int64_t nelem;
	vector<int64_t> size_arr = this_object->m_get_sizes();
	nelem = size_arr[0];
	double* thisdata_arr = this_object->m_get_data();
	if (nelem==0 || (thisdata_arr==NULL)) {
		char buffererr[128];
		sprintf(buffererr, "%s is empty", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
	}
	SEXP r_thisvector = PROTECT(allocVector(REALSXP, nelem));
	double* pr_thisvector = REAL(r_thisvector);
	double* pthisdata_arr = thisdata_arr;
	for (int64_t i=0; i<nelem; i++) {
		pthisdata_arr++;
		pr_thisvector[i] = *pthisdata_arr++;
	}
	UNPROTECT(1);
	return r_thisvector;
}

SEXP construct_PSG_Object(CPSGS_Data_Object* this_object, string& errorstr)
{
	SEXP out = R_NilValue;
	errorstr.clear();
	char buffererr[128];
	switch (this_object->m_get_type()) {
	case PSGS_MATRIX:
		out = PROTECT(construct_matrix(this_object, errorstr));
		break;
	case PSGS_PMATRIX:
		out = PROTECT(construct_pmatrix(this_object, errorstr));
		break;
	case PSGS_POINT:
		out = PROTECT(construct_point(this_object, errorstr));
		break;
	case PSGS_VECTOR:
		out = PROTECT(construct_vector(this_object, errorstr));
		break;
	default:
		sprintf(buffererr, "%s cannot be constructed in R", this_object->m_get_name().c_str());
		errorstr = string(buffererr);
		return R_NilValue;
		break;
	}
	UNPROTECT(1);
	return out;
}

bool encode_rvstring_tostring(SEXP thisrvstring, string& outstr, string& errorstr)
{
	outstr.clear(); errorstr.clear();
	if (!isString(thisrvstring)) {
		errorstr = string("Wrong String");
		return false;
	}
	int num_f = length(thisrvstring);
	outstr=CHAR(STRING_ELT(thisrvstring, 0));
	for (int i=1; i<num_f; i++) {
		string locstr = CHAR(STRING_ELT(thisrvstring, i));
		outstr+=string("\n")+locstr;
	}
	outstr+=string("\n");
	return true;
}



bool encode_RPSG_Problem(SEXP thisproblem, CPSGS_Problem* this_problem, SEXP rho, vector<string>& error_str_arr)
{
	char buffererr[256];
	error_str_arr.clear();
	int count_protect = 0;
	if (!isNewList(thisproblem)) {
		sprintf(buffererr, "Argument should be a list");
		error_str_arr.push_back(string(buffererr));
		return false;
	}
	int num_fields = length(thisproblem); // length of input
	if (num_fields==0) {
		sprintf(buffererr, "Input list is empty");
		error_str_arr.push_back(string(buffererr));
		return false;
	}
	SEXP elmt = R_NilValue;
	SEXP names = PROTECT(getAttrib(thisproblem, R_NamesSymbol));
	count_protect++;
	if (names==R_NilValue) {
		if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
		sprintf(buffererr, "Input list should has named objects");
		error_str_arr.push_back(string(buffererr));
		return false;
	}
	string problem_name, problem_statement;
	char buffer[128];
	char tmpname[128];
	for (int ifild=0; ifild<num_fields; ifild++) {
		const char* thisname = CHAR(STRING_ELT(names, ifild));
		memset(buffer, 0, 128);
		strcpy(buffer, thisname);
		strutil_R::strlwr(buffer);
		memset(tmpname, 0, 128);
		strcpy(tmpname, thisname);
		if (strcmp(buffer, "problem_name")==0) { // problem name
			elmt = VECTOR_ELT(thisproblem, ifild);
			if (!isString(elmt)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "Problem name should be a String");
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			const char* thisproblemname = CHAR(STRING_ELT(elmt, 0));
			problem_name = string(thisproblemname);
			this_problem->m_set_name(problem_name);
			continue;
		}
		else if (strcasecmp(buffer, "problem_statement")==0) { // problem statement
			elmt = VECTOR_ELT(thisproblem, ifild);
			if (!isString(elmt)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "Problem statement should be a String");
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			string errorstr;
			if (!encode_rvstring_tostring(elmt, problem_statement, errorstr)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "Problem statement should be a Vector String");
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			this_problem->m_set_statement(problem_statement);
			continue;
		}
		else if (strstr(buffer, "pmatrix_")==buffer) { // pmatrix !
			int nrow, ncol;
			vector<string> thisvariables_arr;
			vector<int> ivec, jvec;
			vector<double> xvec;
			string errorstr;
			elmt = VECTOR_ELT(thisproblem, ifild);
			if (decode_sparse(elmt, rho, nrow, ncol, thisvariables_arr, ivec, jvec, xvec, errorstr)==R_NilValue) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "%s: %s", tmpname, errorstr.c_str());
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
			vector<int64_t> size_arr;
			size_arr.resize(3); size_arr[0] = nrow; size_arr[1] = ncol; size_arr[2] = xvec.size();
			int64_t nnz = size_arr[2];
			double* thisbody = new double[nnz*3];
			double* pbody = thisbody;
			for (int64_t i=0; i<nnz; i++) {
				*pbody++ = (double)ivec[i];
				*pbody++ = (double)jvec[i];
				*pbody++ = xvec[i];
			}
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(strutil_R::strlwr(tmpname)), PSGS_PMATRIX, &thisvariables_arr, &size_arr, thisbody);
			this_problem->m_add_newdataobject(&this_object);
			delete [] thisbody;
			continue;
		}
		else if (strstr(buffer, "matrix_")==buffer) {
			elmt = VECTOR_ELT(thisproblem, ifild);
			if (!isArray(elmt)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "%s should be a Matrix(RArray)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isNumeric(elmt)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "%s should be a Numeric Matrix(RArray)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			SEXP thisallnames = getAttrib(elmt, R_DimNamesSymbol);
			if (isNull(thisallnames)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "%s should has the Variables Names(Columns Names)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			SEXP thisnames = VECTOR_ELT(thisallnames, 1);
			if (thisnames==R_NilValue) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "%s should has the Variables Names(Columns Names)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isString(thisnames)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "Variables Names should be a String");
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			vector<string> thisvariables_arr;
			const char* thisname;
			for (int iname=0; iname<length(thisnames); iname++) {
				thisname = CHAR(STRING_ELT(thisnames, iname));
				thisvariables_arr.push_back(string(thisname));
			}
			// now get values
			SEXP matrdims;
			matrdims = getAttrib(elmt, R_DimSymbol);
			if (!isVector(matrdims)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "Dimensions for %s not found", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isInteger(matrdims)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "Dimensions for %s should be Integer", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			int nrow = INTEGER(matrdims)[0];
			int ncol = INTEGER(matrdims)[1];
			if (nrow==0 || ncol==0) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "%s is empty", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			PROTECT(elmt); count_protect++;
			bool key_int = false;
			if (isInteger(elmt)) {
				key_int = true;
				elmt = coerceVector(elmt, REALSXP);
			}
			double* thisbody = new double [nrow*ncol];
			double* matrbody = REAL(elmt);
			for (int64_t i=0; i<nrow; i++) {
				for (int64_t j=0; j<ncol; j++) {
					*(thisbody+i*ncol+j) = *(matrbody+j*nrow+i);
				}
			}
			if (key_int) {
				elmt = coerceVector(elmt, INTSXP);
			}
			if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = nrow; size_arr[1] = ncol;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(strutil_R::strlwr(tmpname)), PSGS_MATRIX, &thisvariables_arr, &size_arr, thisbody);
			this_problem->m_add_newdataobject(&this_object);
			delete [] thisbody;
			continue;
		}
		else if (strstr(buffer, "point_")==buffer) { // point
			elmt = VECTOR_ELT(thisproblem, ifild);
			if (!isVector(elmt)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "%s should be a Point(RVector)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isNumeric(elmt)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr, "%s should be a Numeric Point(RVector)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			SEXP thisnames = getAttrib(elmt, R_NamesSymbol);
			if (isNull(thisnames)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr,"%s should has the Variables Names(Columns Names)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isString(thisnames)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr,"%s: Variables Names should be a String", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			vector<string> thisvariables_arr;
			const char* thisname;
			for (int iname=0; iname<length(thisnames); iname++) {
				thisname = CHAR(STRING_ELT(thisnames, iname));
				thisvariables_arr.push_back(string(thisname));
			}
			int numelmnt = length(elmt);
			PROTECT(elmt); count_protect++;
			bool key_int = false;
			if (isInteger(elmt)) {
				key_int = true;
				elmt = coerceVector(elmt, REALSXP);
			}
			double* thisbody = new double [numelmnt];
			double* pointbody = REAL(elmt);
			for (int64_t i=0; i<numelmnt; i++) {
				*(thisbody+i) = *(pointbody+i);
			}
			if (key_int) {
				elmt = coerceVector(elmt, INTSXP);
			}
			if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = numelmnt; size_arr[1] = 1;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(strutil_R::strlwr(tmpname)), PSGS_POINT, &thisvariables_arr, &size_arr, pointbody);
			this_problem->m_add_newdataobject(&this_object);
			delete [] thisbody;
			continue;
		}
		else if (strstr(buffer, "vector_")==buffer) { // vector
			elmt = VECTOR_ELT(thisproblem, ifild);
			if (!isVector(elmt)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr,"%s should be a Vector(RVector)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isNumeric(elmt)) {
				if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
				sprintf(buffererr,"%s should be a Numeric Vector(RVector)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			int numelmnt = length(elmt);
			PROTECT(elmt); count_protect++;
			bool key_int = false;
			if (isInteger(elmt)) {
				key_int = true;
				elmt = coerceVector(elmt, REALSXP);
			}
			double* thisbody = new double [2*numelmnt];
			double* pointbody = REAL(elmt);
			double* pthisbody = thisbody;
			double* ppointbody = pointbody;
			for (int64_t i=0; i<numelmnt; i++) {
				*pthisbody++ = i+1;
				*pthisbody++ = *ppointbody++;
			}
			if (key_int) {
				elmt = coerceVector(elmt, INTSXP);
			}
			if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
			vector<string> header_arr;
			header_arr.push_back("id"); header_arr.push_back("value");
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = numelmnt; size_arr[1] = 2;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(tmpname, PSGS_VECTOR, &header_arr, &size_arr, thisbody);
			this_problem->m_add_newdataobject(&this_object);
			delete [] thisbody;
			continue;
		}
	}
	if (count_protect) {UNPROTECT(count_protect); count_protect = 0;}
	return true;
}

bool encode_RPSG_Function(SEXP thisfunclist, CPSGS_Problem* this_problem, SEXP rho, vector<string>& error_str_arr)
{
	char buffererr[256];
	error_str_arr.clear();
	if (!isNewList(thisfunclist)) {
		sprintf(buffererr, "Argument should be a list");
		error_str_arr.push_back(string(buffererr));
		return false;
	}
	int num_fields = length(thisfunclist); // length of input
	if (num_fields==0) {
		sprintf(buffererr, "Input list is empty");
		error_str_arr.push_back(string(buffererr));
		return false;
	}
	SEXP elmt = R_NilValue;
	SEXP names = PROTECT(getAttrib(thisfunclist, R_NamesSymbol));
	if (names==R_NilValue) {
		UNPROTECT(1);
		sprintf(buffererr, "Input list should has named objects");
		error_str_arr.push_back(string(buffererr));
		return false;
	}
	string problem_name, problem_statement;
	char buffer[128];
	char tmpname[128];
	for (int ifild=0; ifild<num_fields; ifild++) {
		const char* thisname = CHAR(STRING_ELT(names, ifild));
		memset(buffer, 0, 128);
		strcpy(buffer, thisname);
		strutil_R::strlwr(buffer);
		memset(tmpname, 0, 128);
		strcpy(tmpname, thisname);
		if (strcmp(buffer, "function_description")==0) { // problem name
			elmt = VECTOR_ELT(thisfunclist, ifild);
			if (!isString(elmt)) {
				UNPROTECT(1);
				sprintf(buffererr, "Function description should be a String");
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			const char* thisfuncdescr = CHAR(STRING_ELT(elmt, 0));
			problem_statement = string(thisfuncdescr);
			this_problem->m_set_statement(problem_statement);
			problem_name = string("problem_calcfunction");
			this_problem->m_set_name(problem_name);
			continue;
		}
		else if (strstr(buffer, "pmatrix_")==buffer) { // pmatrix !
			int nrow, ncol;
			vector<string> thisvariables_arr;
			vector<int> ivec, jvec;
			vector<double> xvec;
			string errorstr;
			elmt = VECTOR_ELT(thisfunclist, ifild);
			if (decode_sparse(elmt, rho, nrow, ncol, thisvariables_arr, ivec, jvec, xvec, errorstr)==R_NilValue) {
				UNPROTECT(1);
				sprintf(buffererr, "%s: %s", tmpname, errorstr.c_str());
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			UNPROTECT(1);
			vector<int64_t> size_arr;
			size_arr.resize(3); size_arr[0] = nrow; size_arr[1] = ncol; size_arr[2] = xvec.size();
			int64_t nnz = size_arr[2];
			double* thisbody = new double[nrow*3];
			double* pbody = thisbody;
			for (int64_t i=0; i<nnz; i++) {
				*pbody++ = (double)ivec[i];
				*pbody++ = (double)jvec[i];
				*pbody++ = xvec[i];
			}
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(strutil_R::strlwr(tmpname)), PSGS_PMATRIX, &thisvariables_arr, &size_arr, thisbody);
			this_problem->m_add_newdataobject(&this_object);
			delete [] thisbody;
			continue;
		}
		else if (strstr(buffer, "matrix_")==buffer) { // matrix
			elmt = VECTOR_ELT(thisfunclist, ifild);
			if (!isArray(elmt)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should be a Matrix(RArray)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isNumeric(elmt)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should be a Numeric Matrix(RArray)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			SEXP thisallnames = getAttrib(elmt, R_DimNamesSymbol);
			if (isNull(thisallnames)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should has the Variables Names(Columns Names)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			SEXP thisnames = VECTOR_ELT(thisallnames, 1);
			if (thisnames==R_NilValue) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should has the Variables Names(Columns Names)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isString(thisnames)) {
				UNPROTECT(1);
				sprintf(buffererr, "Variables Names should be a String");
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			vector<string> thisvariables_arr;
			const char* thisname;
			for (int iname=0; iname<length(thisnames); iname++) {
				thisname = CHAR(STRING_ELT(thisnames, iname));
				thisvariables_arr.push_back(string(thisname));
			}
			SEXP matrdims;
			matrdims = getAttrib(elmt, R_DimSymbol);
			if (!isVector(matrdims)) {
				UNPROTECT(1);
				sprintf(buffererr, "Dimensions for %s not found", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isInteger(matrdims)) {
				UNPROTECT(1);
				sprintf(buffererr, "Dimensions for %s should be Integer", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			int nrow = INTEGER(matrdims)[0];
			int ncol = INTEGER(matrdims)[1];
			if (nrow==0 || ncol==0) {
				UNPROTECT(1);
				sprintf(buffererr, "%s is empty", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			PROTECT(elmt);
			bool key_int = false;
			if (isInteger(elmt)) {
				key_int = true;
				elmt = coerceVector(elmt, REALSXP);
			}
			double* thisbody = new double [nrow*ncol];
			double* matrbody = REAL(elmt);
			for (int64_t i=0; i<nrow; i++) {
				for (int64_t j=0; j<ncol; j++) {
					*(thisbody+i*ncol+j) = *(matrbody+j*nrow+i);
				}
			}
			if (key_int) {
				elmt = coerceVector(elmt, INTSXP);
			}
			UNPROTECT(1);
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = nrow; size_arr[1] = ncol;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(strutil_R::strlwr(tmpname)), PSGS_MATRIX, &thisvariables_arr, &size_arr, thisbody);
			this_problem->m_add_newdataobject(&this_object);
			delete [] thisbody;
			continue;
		}
		else if (strstr(buffer, "point_")==buffer) { // point
			elmt = VECTOR_ELT(thisfunclist, ifild);
			if (!isVector(elmt)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should be a Point(RVector)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isNumeric(elmt)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should be a Numeric Point(RVector)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			SEXP thisnames = getAttrib(elmt, R_NamesSymbol);
			if (isNull(thisnames)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should has the Variables Names(Columns Names)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isString(thisnames)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s: Variables Names should be a String", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			vector<string> thisvariables_arr;
			const char* thisname;
			for (int iname=0; iname<length(thisnames); iname++) {
				thisname = CHAR(STRING_ELT(thisnames, iname));
				thisvariables_arr.push_back(string(thisname));
			}
			int numelmnt = length(elmt);
			PROTECT(elmt);
			bool key_int = false;
			if (isInteger(elmt)) {
				key_int = true;
				elmt = coerceVector(elmt, REALSXP);
			}
			double* thisbody = new double [numelmnt];
			double* pointbody = REAL(elmt);
			for (int64_t i=0; i<numelmnt; i++) {
				*(thisbody+i) = *(pointbody+i);
			}
			if (key_int) {
				elmt = coerceVector(elmt, INTSXP);
			}
			UNPROTECT(1);
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = numelmnt; size_arr[1] = 1;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(string(strutil_R::strlwr(tmpname)), PSGS_POINT, &thisvariables_arr, &size_arr, pointbody);
			this_problem->m_add_newdataobject(&this_object);
			delete [] thisbody;
			continue;
		}
		else if (strstr(buffer, "vector_")==buffer) { // vector
			elmt = VECTOR_ELT(thisfunclist, ifild);
			if (!isVector(elmt)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should be a Vector(RVector)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			if (!isNumeric(elmt)) {
				UNPROTECT(1);
				sprintf(buffererr, "%s should be a Numeric Vector(RVector)", tmpname);
				error_str_arr.push_back(string(buffererr));
				return false;
			}
			int numelmnt = length(elmt);
			PROTECT(elmt);
			bool key_int = false;
			if (isInteger(elmt)) {
				key_int = true;
				elmt = coerceVector(elmt, REALSXP);
			}
			double* thisbody = new double [2*numelmnt];
			double* pointbody = REAL(elmt);
			double* pthisbody = thisbody;
			double* ppointbody = pointbody;
			for (int64_t i=0; i<numelmnt; i++) {
				*pthisbody++ = i+1;
				*pthisbody++ = *ppointbody++;
			}
			if (key_int) {
				elmt = coerceVector(elmt, INTSXP);
			}
			UNPROTECT(1);
			vector<string> header_arr;
			header_arr.push_back("id"); header_arr.push_back("value");
			vector<int64_t> size_arr;
			size_arr.resize(2); size_arr[0] = numelmnt; size_arr[1] = 2;
			CPSGS_Data_Object* this_object = new CPSGS_Data_Object(tmpname, PSGS_VECTOR, &header_arr, &size_arr, thisbody);
			this_problem->m_add_newdataobject(&this_object);
			delete [] thisbody;
			continue;
		}
	}
	UNPROTECT(1);
	return true;
}

SEXP construct_PSG_Problem_Output(CPSGS_OutputProblem* this_out_problem, vector<string>& error_str_arr, bool& res)
{
	SEXP outlist = R_NilValue;
	SEXP fieldnames = R_NilValue;
	int num_fieds_in_list = 0;
	error_str_arr.clear();
	string problem_name = this_out_problem->m_get_name();
	num_fieds_in_list++;
	// solution status
	string solution_status = decode_solution_status(this_out_problem->m_get_solution_status());
	num_fieds_in_list++;
	// problem_statement
	vector<std::string> problemstatementstr_arr;
	if (this_out_problem->m_get_statement_asvector(problemstatementstr_arr) != 0) num_fieds_in_list++;
	// ouputs
	vector<std::string>* outstr_arr = this_out_problem->m_get_output();
	if (!outstr_arr->empty()) num_fieds_in_list++;
	// points
	vector<CPSGS_Data_Object*> this_points_array;
	int num_points = this_out_problem->m_get_dataobjects(PSGS_POINT, this_points_array);
	num_fieds_in_list += num_points;
	// vectors
	vector<CPSGS_Data_Object*> this_vectors_array;
	int num_vectors = this_out_problem->m_get_dataobjects(PSGS_VECTOR, this_vectors_array);
	num_fieds_in_list += num_vectors;
	// matrices
	vector<CPSGS_Data_Object*> this_matrices_array;
	int num_matrices = this_out_problem->m_get_dataobjects(PSGS_MATRIX, this_matrices_array);
	num_fieds_in_list += num_matrices;
	// log
	vector<std::string>* logstr_arr = this_out_problem->m_get_log();
	if (!logstr_arr->empty()) num_fieds_in_list++;
	// errors
	vector<std::string>* errorsstr_arr = this_out_problem->m_get_errors();
	if (!errorsstr_arr->empty()) num_fieds_in_list++;
	// warnings
	vector<std::string>* warningsstr_arr = this_out_problem->m_get_warnings();
	if (!warningsstr_arr->empty()) num_fieds_in_list++;
	// collect data to R list
	outlist = PROTECT(allocVector(VECSXP, num_fieds_in_list));
	fieldnames = PROTECT(allocVector(STRSXP, num_fieds_in_list));
	int curr_field_num = 0;
	res = true;
	string errorstr;
	// problem_name
	SEXP outproblem_name = PROTECT(construct_string(problem_name, errorstr));
	if (outproblem_name==R_NilValue) {
		SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
		SET_STRING_ELT(fieldnames, curr_field_num, mkChar("problem_name"));
		curr_field_num++;
		error_str_arr.push_back(errorstr);
		res = false;
	}
	else {
		SET_VECTOR_ELT(outlist, curr_field_num, outproblem_name);
		SET_STRING_ELT(fieldnames, curr_field_num, mkChar("problem_name"));
		curr_field_num++;
	}
	UNPROTECT(1);
	// solution status
	SEXP outsolution_status = PROTECT(construct_string(solution_status, errorstr));
	if (outsolution_status==R_NilValue) {
		SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
		SET_STRING_ELT(fieldnames, curr_field_num, mkChar("solution_status"));
		curr_field_num++;
		error_str_arr.push_back(errorstr);
		res = false;
	}
	else {
		SET_VECTOR_ELT(outlist, curr_field_num, outsolution_status);
		SET_STRING_ELT(fieldnames, curr_field_num, mkChar("solution_status"));
		curr_field_num++;
	}
	UNPROTECT(1);
	if (!problemstatementstr_arr.empty()) {
		SEXP prstatoutputs = PROTECT(construct_string_array(&problemstatementstr_arr, errorstr));
		if (prstatoutputs==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("problem_statement"));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, prstatoutputs);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("problem_statement"));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// outputs
	if (!outstr_arr->empty()) {
		SEXP outoutputs = PROTECT(construct_string_array(outstr_arr, errorstr));
		if (outoutputs==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("output"));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outoutputs);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("output"));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// points
	for (size_t i=0; i<this_points_array.size(); i++) {
		SEXP outpoint = PROTECT(construct_PSG_Object(this_points_array[i], errorstr));
		if (outpoint==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_points_array[i]->m_get_name().c_str()));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outpoint);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_points_array[i]->m_get_name().c_str()));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// vectors
	for (size_t i=0; i<this_vectors_array.size(); i++) {
		SEXP outvector = PROTECT(construct_PSG_Object(this_vectors_array[i], errorstr));
		if (outvector==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_vectors_array[i]->m_get_name().c_str()));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outvector);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_vectors_array[i]->m_get_name().c_str()));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// matrices
	for (size_t i=0; i<this_matrices_array.size(); i++) {
		SEXP outmatrix = PROTECT(construct_PSG_Object(this_matrices_array[i], errorstr));
		if (outmatrix==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_matrices_array[i]->m_get_name().c_str()));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outmatrix);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_matrices_array[i]->m_get_name().c_str()));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// errors
	if (!errorsstr_arr->empty()) {
		SEXP outerrors = PROTECT(construct_string_array(errorsstr_arr, errorstr));
		if (outerrors==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("errors"));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outerrors);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("errors"));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// warnings
	if (!warningsstr_arr->empty()) {
		SEXP outwarnings = PROTECT(construct_string_array(warningsstr_arr, errorstr));
		if (outwarnings==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("warnings"));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outwarnings);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("warnings"));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// log
	if (!logstr_arr->empty()) {
		SEXP outlog = PROTECT(construct_string_array(logstr_arr, errorstr));
		if (outlog==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("log"));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outlog);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("log"));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	setAttrib(outlist, R_NamesSymbol, fieldnames);
	UNPROTECT(2);
	return outlist;
}

SEXP construct_PSG_Problem_Direct(CPSGS_Problem* this_problem, vector<string>& error_str_arr, bool& res)
{
	SEXP outlist = R_NilValue;
	SEXP fieldnames = R_NilValue;
	int num_fieds_in_list = 0;
	error_str_arr.clear();
	// problem_name
	string problem_name = this_problem->m_get_name();
	num_fieds_in_list++;
	// problem_statement
	vector<std::string> problemstatementstr_arr;
	if (this_problem->m_get_statement_asvector(problemstatementstr_arr) != 0) num_fieds_in_list++;
	// points
	vector<CPSGS_Data_Object*> this_points_array;
	int num_points = this_problem->m_get_dataobjects(PSGS_POINT, this_points_array);
	num_fieds_in_list += num_points;
	// vectors
	vector<CPSGS_Data_Object*> this_vectors_array;
	int num_vectors = this_problem->m_get_dataobjects(PSGS_VECTOR, this_vectors_array);
	num_fieds_in_list += num_vectors;
	// matrices
	vector<CPSGS_Data_Object*> this_matrices_array;
	int num_matrices = this_problem->m_get_dataobjects(PSGS_MATRIX, this_matrices_array);
	num_fieds_in_list += num_matrices;
	// pmatrices
	vector<CPSGS_Data_Object*> this_pmatrices_array;
	int num_pmatrices = this_problem->m_get_dataobjects(PSGS_PMATRIX, this_pmatrices_array);
	num_fieds_in_list += num_pmatrices;
	// collect data to R list
	outlist = PROTECT(allocVector(VECSXP, num_fieds_in_list));
	fieldnames = PROTECT(allocVector(STRSXP, num_fieds_in_list));
	int curr_field_num = 0;
	res = true;
	string errorstr;
	// problem_name
	SEXP outproblem_name = PROTECT(construct_string(problem_name, errorstr));
	if (outproblem_name==R_NilValue) {
		SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
		SET_STRING_ELT(fieldnames, curr_field_num, mkChar("problem_name"));
		curr_field_num++;
		error_str_arr.push_back(errorstr);
		res = false;
	}
	else {
		SET_VECTOR_ELT(outlist, curr_field_num, outproblem_name);
		SET_STRING_ELT(fieldnames, curr_field_num, mkChar("problem_name"));
		curr_field_num++;
	}
	UNPROTECT(1);
	// problem_statement
	if (!problemstatementstr_arr.empty()) {
		SEXP prstatoutputs = PROTECT(construct_string_array(&problemstatementstr_arr, errorstr));
		if (prstatoutputs==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("problem_statement"));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, prstatoutputs);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar("problem_statement"));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// points
	for (size_t i=0; i<this_points_array.size(); i++) {
		SEXP outpoint = PROTECT(construct_PSG_Object(this_points_array[i], errorstr));
		if (outpoint==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_points_array[i]->m_get_name().c_str()));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outpoint);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_points_array[i]->m_get_name().c_str()));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// vectors
	for (size_t i=0; i<this_vectors_array.size(); i++) {
		SEXP outvector = PROTECT(construct_PSG_Object(this_vectors_array[i], errorstr));
		if (outvector==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_vectors_array[i]->m_get_name().c_str()));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outvector);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_vectors_array[i]->m_get_name().c_str()));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// matrices
	for (size_t i=0; i<this_matrices_array.size(); i++) {
		SEXP outmatrix = PROTECT(construct_PSG_Object(this_matrices_array[i], errorstr));
		if (outmatrix==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_matrices_array[i]->m_get_name().c_str()));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outmatrix);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_matrices_array[i]->m_get_name().c_str()));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	// pmatrices
	for (size_t i=0; i<this_pmatrices_array.size(); i++) {
		SEXP outpmatrix = PROTECT(construct_PSG_Object(this_pmatrices_array[i], errorstr));
		if (outpmatrix==R_NilValue) {
			SET_VECTOR_ELT(outlist, curr_field_num, R_NilValue);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_pmatrices_array[i]->m_get_name().c_str()));
			curr_field_num++;
			error_str_arr.push_back(errorstr);
			res = false;
		}
		else {
			SET_VECTOR_ELT(outlist, curr_field_num, outpmatrix);
			SET_STRING_ELT(fieldnames, curr_field_num, mkChar(this_pmatrices_array[i]->m_get_name().c_str()));
			curr_field_num++;
		}
		UNPROTECT(1);
	}
	setAttrib(outlist, R_NamesSymbol, fieldnames);
	UNPROTECT(2);
	return outlist;
}

SEXP lookforExternelObject(string objectname, SEXP rho)
{
	SEXP varname;
	PROTECT(varname = allocVector(STRSXP, 1));
	SET_STRING_ELT(varname, 0, mkChar(objectname.c_str()));
	SEXP extvar = PROTECT(findVar(installChar(STRING_ELT(varname, 0)), rho));
	if (extvar == R_UnboundValue) {
		extvar = findVar(installChar(STRING_ELT(varname, 0)), R_GlobalEnv);
	}
	UNPROTECT(2);
	if (extvar == R_UnboundValue) { // find original named object
		
	}
	return extvar;
}



SEXP lookforExternelObjectExt(string objectname, string statement, SEXP rho, string& errorstr)
{
	errorstr.clear();
	SEXP varname;
	PROTECT(varname = allocVector(STRSXP, 1));
	SET_STRING_ELT(varname, 0, mkChar(objectname.c_str()));
	SEXP extvar = PROTECT(findVar(installChar(STRING_ELT(varname, 0)), rho));
	if (extvar == R_UnboundValue) {
		extvar = findVar(installChar(STRING_ELT(varname, 0)), R_GlobalEnv);
	}
	UNPROTECT(2);
	if (extvar == R_UnboundValue) { // find original named object
		// find original names 
		vector<string> original_names;
		int res = find_original_object_name(objectname, statement, original_names);
		if (res == 0) {
			char errbuffer[256];
			sprintf(errbuffer, "%s not found in Problem Statement", objectname.c_str());
			errorstr = string(errbuffer);
			return R_UnboundValue;
		}
		else if (res > 1) {
			char errbuffer[1024];
			char locbuffer[1024];
			sprintf(errbuffer, "Doubling of objects names for %s (%s", objectname.c_str(), original_names[0].c_str());
			for (size_t i=1; i<original_names.size(); i++) {
				sprintf(locbuffer, ", %s", original_names[i].c_str());
				strcat(errbuffer, locbuffer);
			}
			sprintf(locbuffer, ")");
			strcat(errbuffer, locbuffer);
			errorstr = string(errbuffer);
			return R_UnboundValue;
		}
		SEXP orgvarname;
		PROTECT(orgvarname = allocVector(STRSXP, 1));
		SET_STRING_ELT(orgvarname, 0, mkChar(original_names[0].c_str()));
		SEXP orgextvar = PROTECT(findVar(installChar(STRING_ELT(orgvarname, 0)), rho));
		if (orgextvar == R_UnboundValue) {
			orgextvar = findVar(installChar(STRING_ELT(orgvarname, 0)), R_GlobalEnv);
		}
		if (orgextvar == R_UnboundValue) {
			char errbuffer[256];
			sprintf(errbuffer, "%s not found in R Environment", objectname.c_str());
			errorstr = string(errbuffer);
		}
		UNPROTECT(2);
		return orgextvar;
	}
	return extvar;
}

int split_text_to_vector(const char* inpstr, vector<string>& out_str_arr)
{
	out_str_arr.clear();
	if (strlen(inpstr)==0) return 0;
	char* inpbuffer = new char[strlen(inpstr)*sizeof(char)];
	memset(inpbuffer, 0, sizeof(char)*strlen(inpstr));
	inpbuffer = strdup(inpstr);
	char seps[]   = " ()*-,\t\r\n;:=[]{}#&";
	char *token, *next_token;
	token = strtok_r(inpbuffer, seps, &next_token);
	while (token != NULL)	{
		out_str_arr.push_back(string(token));
		token = strtok_r(NULL, seps, &next_token);
	}
	free(inpbuffer);
	return (int)out_str_arr.size();
}

int split_text_to_vector_light(const char* inpstr, vector<string>& out_str_arr)
{
	out_str_arr.clear();
	if (strlen(inpstr)==0) return 0;
	char* inpbuffer = new char[strlen(inpstr)*sizeof(char)];
	memset(inpbuffer, 0, sizeof(char)*strlen(inpstr));
	inpbuffer = strdup(inpstr);
	char seps[]   = " \t";
	char *token, *next_token;
	token = strtok_r(inpbuffer, seps, &next_token);
	while (token != NULL)	{
		out_str_arr.push_back(string(token));
		token = strtok_r(NULL, seps, &next_token);
	}
	free(inpbuffer);
	return (int)out_str_arr.size();
}

int find_original_object_name(string objectname, string statement, vector<string>& original_names)
{
	original_names.clear();
	vector<string> lexems_str_arr;
	if (split_text_to_vector(statement.c_str(), lexems_str_arr)==0)	return 0;
	for (size_t i=0; i<lexems_str_arr.size(); i++) {
		if (toLower(lexems_str_arr[i])==objectname) {
			if (find(original_names.begin(), original_names.end(), lexems_str_arr[i])==original_names.end()) {
				original_names.push_back(lexems_str_arr[i]);
			}
		}
	}
	return (int)original_names.size();
}

int mytolower(int c) {return tolower(c);}

bool check_and_correct_pathimportexport(string& path_toexport, bool key_import, string& thisproblem_name, string& errorstr)
{
	errorstr.clear();
	string locpath_toexport, root_path, file_name, problem_name;
	locpath_toexport = path_toexport;
	if (!get_path_from_filename((const string)locpath_toexport, root_path, file_name)) {
		errorstr = string("Wrong export path\n");
		return false;
	}
	if (file_name.empty() && (!key_import)) {
		file_name = string("problem_1.txt");
		thisproblem_name = string("problem_1");
		path_toexport += file_name;
	}
	string locproblem_name = toLower(file_name);
	size_t pos = locproblem_name.find("problem_");
	if (pos == 0) {
		pos = locproblem_name.find(".txt");
		if (pos != string::npos) {
			thisproblem_name = file_name.substr(0, pos);
			return true;
		}
		thisproblem_name = file_name;
		path_toexport += string(".txt");
		return true;
	}
	else {
		if (key_import) {
			thisproblem_name.clear();
			errorstr = string("Problem file not defined");
			return false;
		}
	}
	if ((*root_path.rbegin() != '\\') && (*root_path.rbegin() != '/')) root_path += string("/");
	path_toexport = root_path + thisproblem_name + string(".txt");
	return true;
}


extern "C"  SEXP  crpsg_importfromtext(SEXP rpathtostatement, SEXP suppressMessages)
{
	string errorstr;
	if (!isString(rpathtostatement)) {
		Rprintf("path of import should be a String\n");
		return R_NilValue;
	}
	string path_import;
	path_import = string(CHAR(STRING_ELT(rpathtostatement, 0)));
	if (path_import.empty()) {
		Rprintf("path is empty\n");
		return R_NilValue;
	}
	string problem_name_fromfile;
	if (!check_and_correct_pathimportexport(path_import, true, problem_name_fromfile, errorstr)) {
		Rprintf("%s\n", errorstr.c_str());
		return R_NilValue;
	}
	if (!isLogical(suppressMessages)) {
		Rprintf("suppressMessages must be Logical\n");
		return R_NilValue;
	}
	bool key_suppressMessages = LOGICAL(suppressMessages)[0];
	CPSGS_Problem* this_problem = new CPSGS_Problem();
	CRPSGSolverHelper* thisCRPSGSolverHelper = new CRPSGSolverHelper();
	if (!thisCRPSGSolverHelper->m_set_R_Functions(&PrintToR, &ProcessStopToR, &CycleFinishToR, &ProcessCancelToR,
			&RunRExternalFunction, &RunRGradientExternalFunction,
			&RunRExternalFunctionDir, &RunRGradientExternalFunctionDir)) {
		thisCRPSGSolverHelper->OnError(-1, "Import Problem from text", "Implicit Functions was not set");
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	thisCRPSGSolverHelper->m_set_suppressMessages(key_suppressMessages);
	thisCRPSGSolverHelper->m_set_problem(this_problem);
	if (CRPSGSolverHelper::ImportProblemFromTextFiles(path_import, thisCRPSGSolverHelper)<0) {
		thisCRPSGSolverHelper->OnError(0, "Import Problem from text", "Problem was not imported");
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	bool res;
	vector<string> error_str_arr;
	SEXP outlist = PROTECT(construct_PSG_Problem_Direct(this_problem, error_str_arr, res));
	if (!res) {
		char buffererr[128];
		for (size_t i=0; i<error_str_arr.size(); i++) {
			sprintf(buffererr, "%s\n", error_str_arr[i].c_str());
			thisCRPSGSolverHelper->OnError(0, "Import Problem from text", buffererr);
		}
		UNPROTECT(1);
		thisCRPSGSolverHelper->OnError(0, "Import Problem from text", "Problem was not imported");
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	else {
		Rprintf("OK. Problem Imported\n\n");
		UNPROTECT(1);
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return outlist;
	}
	return outlist;
}

extern "C"  SEXP  crpsg_getfunctionvalue(SEXP regime, SEXP thisfunclist, SEXP rho, SEXP allowExt, SEXP suppressMessages, SEXP allowFunVal)
{
	if (!isInteger(regime)) {
		Rprintf("regime must be Integer\n");
		return R_NilValue;
	}
	RPSG_REGIME_TYPE this_rpsg_regime = RPSG_REGIME_UNKNOWN;
	switch (INTEGER(regime)[0]) {
		case 0: this_rpsg_regime = RPSG_REGIME_GETFUNCVALUE; break;
		case 1: this_rpsg_regime = RPSG_REGIME_GETFUNCINCR; break;
		case 2: this_rpsg_regime = RPSG_REGIME_GETFUNCSENS; break;
		default: this_rpsg_regime = RPSG_REGIME_UNKNOWN; break;
	}
	if (this_rpsg_regime == RPSG_REGIME_UNKNOWN) {
		Rprintf("Unknown Regime\n");
		return R_NilValue;
	}
	if (!isLogical(allowExt)) {
		Rprintf("allowExt must be Logical\n");
		return R_NilValue;
	}
	bool key_allowExt = LOGICAL(allowExt)[0];
	if (!isLogical(suppressMessages)) {
		Rprintf("suppressMessages must be Logical\n");
		return R_NilValue;
	}
	bool key_suppressMessages = LOGICAL(suppressMessages)[0];
	if (!isLogical(allowFunVal)) {
		Rprintf("allowFunVal must be Logical\n");
		return R_NilValue;
	}
	bool key_allowFunVal = LOGICAL(allowFunVal)[0];
	if (!isEnvironment(rho)) {
		Rprintf("Environment not found\n");
		return R_NilValue;
	}
	if (!isNewList(thisfunclist)) {
		Rprintf("Argument should be a list\n");
		return R_NilValue;
	}
	int num_fields = length(thisfunclist); // length of input
	if (num_fields==0) {
		Rprintf("Input list is empty\n");
		return R_NilValue;
	}
	CPSGS_Problem* this_problem = new CPSGS_Problem();
	vector<string> error_str_arr;
	// collect functions data
	string point_argumentname("point_argument");
	if (!encode_RPSG_Function(thisfunclist, this_problem, rho, error_str_arr)) {
		delete this_problem;
		for (size_t i=0; i<error_str_arr.size(); i++) {
			Rprintf("%s\n", error_str_arr[i].c_str());
		}
		Rprintf("Function cannot be evaluated\n");
		return R_NilValue;
	}
	// check data
	CRPSGSolverHelper* thisCRPSGSolverHelper = new CRPSGSolverHelper();
	if (!thisCRPSGSolverHelper->m_set_R_Functions(&PrintToR, &ProcessStopToR, &CycleFinishToR, &ProcessCancelToR,
			&RunRExternalFunction, &RunRGradientExternalFunction,
			&RunRExternalFunctionDir, &RunRGradientExternalFunctionDir)) {
		Rprintf("Implicit Functions was not set\n");
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	thisCRPSGSolverHelper->m_set_suppressMessages(key_suppressMessages);
	thisCRPSGSolverHelper->m_set_problem(this_problem);

	vector<CPSGS_OutputProblem*>* this_output_collection = this_problem->m_get_output_problem_collection();
	if (this_output_collection->empty()) {
		CPSGS_OutputProblem* this_new_output = new CPSGS_OutputProblem();
		this_new_output->m_set_name(this_problem->m_get_name());
		this_problem->m_set_curr_output_problem(this_new_output);
		this_output_collection->push_back(this_new_output);
	}
	string errorstr;
	vector<string> pointfs_arr, vectorfs_arr, matrixfs_arr, pmatrixfs_arr, added_objects_arr;
	string problem_statement_loc = string("calculate\nvalue:\n")+this_problem->m_get_statement();
	if (CRPSGSolverHelper::GetPSGDataObjectsFromStatement(problem_statement_loc, thisCRPSGSolverHelper, pointfs_arr, vectorfs_arr, matrixfs_arr, pmatrixfs_arr, added_objects_arr, errorstr)<=0) {
		thisCRPSGSolverHelper->OnError(0, "PSG Function", errorstr.c_str());
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	// check others
	vector<string> lost_obj_arr;
	thisCRPSGSolverHelper->IsProblemHasAllInputData(&pointfs_arr, &vectorfs_arr, &matrixfs_arr, &pmatrixfs_arr, lost_obj_arr);
	if (lost_obj_arr.size()>0) {
		bool key_OK = false;
		size_t num_fieds_in_addlist = lost_obj_arr.size();
		if (key_allowExt && (num_fieds_in_addlist!=0)) { // try to find External
			error_str_arr.clear();
			key_OK = true;
			SEXP addinplist = PROTECT(allocVector(VECSXP, num_fieds_in_addlist));
			SEXP addfieldnames = PROTECT(allocVector(STRSXP, num_fieds_in_addlist));
			string errorstrloc;
			for (size_t i=0; i<num_fieds_in_addlist; i++) {
				SEXP rextobj =  PROTECT(lookforExternelObjectExt(lost_obj_arr[i], this_problem->m_get_statement(), rho, errorstrloc));
				if (rextobj == R_UnboundValue) {
					error_str_arr.push_back(errorstrloc);
					UNPROTECT(1);
					key_OK = false;
					continue;
				}
				else {
					SET_VECTOR_ELT(addinplist, i, rextobj);
					SET_STRING_ELT(addfieldnames, i, mkChar(lost_obj_arr[i].c_str()));
					UNPROTECT(1);
				}
			}
			setAttrib(addinplist, R_NamesSymbol, addfieldnames);
			if (key_OK) { // add objects
				error_str_arr.clear();
				key_OK = encode_RPSG_Function(addinplist, this_problem, rho, error_str_arr);
			}
			UNPROTECT(2);
		}
		else {
			error_str_arr = lost_obj_arr;
		}
	}
	// now calculate values
	if (this_rpsg_regime == RPSG_REGIME_GETFUNCVALUE) {
		double value;
		int ret = CRPSGSolverHelper::get_Function_Value_1(thisCRPSGSolverHelper, this_problem->m_get_statement().c_str(), "point_argument", &value);
		if (ret != 0) {
			char buffererr[128];
			sprintf(buffererr, "Value of %s not found", this_problem->m_get_statement().c_str());
			thisCRPSGSolverHelper->OnError(0, "PSG Function", buffererr);
			delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
			return R_NilValue;
		}
		else {
			string errorstr;
			SEXP outres = PROTECT(construct_real(&value, 1, errorstr));
			UNPROTECT(1);
			return outres;
		}
	}
	else if (this_rpsg_regime == RPSG_REGIME_GETFUNCINCR) {
		double fValue;
		CPSGS_Data_Object* this_point;
		int ret=CRPSGSolverHelper::get_Function_Increment_1(thisCRPSGSolverHelper,  this_problem->m_get_statement().c_str(), "point_argument",
			&this_point, &fValue);
		if (ret != 0) {
			char buffererr[128];
			sprintf(buffererr, "Increment for %s not found", this_problem->m_get_statement().c_str());
			thisCRPSGSolverHelper->OnError(0, "PSG Function", buffererr);
			delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
			return R_NilValue;
		}
		else {
			string errorstr;
			SEXP outpoint = PROTECT(construct_point(this_point, errorstr));
			if (!key_allowFunVal) {
				UNPROTECT(1);
				return outpoint;
			}
			else {
				SEXP outfunval = PROTECT(construct_real(&fValue, 1, errorstr));
				SEXP outlist = PROTECT(allocVector(VECSXP, 2));
				SEXP fieldnames = PROTECT(allocVector(STRSXP, 2));
				SET_VECTOR_ELT(outlist, 0, outfunval);
				SET_STRING_ELT(fieldnames, 0, mkChar("Fun_Value"));
				SET_VECTOR_ELT(outlist, 1, outpoint);
				SET_STRING_ELT(fieldnames, 1, mkChar("Fun_Increment"));
				setAttrib(outlist, R_NamesSymbol, fieldnames);
				UNPROTECT(4);
				return outlist;
			}
		}
	}
	else if (this_rpsg_regime == RPSG_REGIME_GETFUNCSENS) {
		double fValue;
		CPSGS_Data_Object* this_point;
		int ret=CRPSGSolverHelper::get_Function_Gradient_1(thisCRPSGSolverHelper,  this_problem->m_get_statement().c_str(), "point_argument", &this_point, &fValue);
		if (ret != 0) {
			char buffererr[128];
			sprintf(buffererr, "Sensetivity for %s not found", this_problem->m_get_statement().c_str());
			thisCRPSGSolverHelper->OnError(0, "PSG Function", buffererr);
			delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
			return R_NilValue;
		}
		else {
			string errorstr;
			SEXP outpoint = PROTECT(construct_point(this_point, errorstr));
			if (!key_allowFunVal) {
				UNPROTECT(1);
				return outpoint;
			}
			else {
				SEXP outfunval = PROTECT(construct_real(&fValue, 1, errorstr));
				SEXP outlist = PROTECT(allocVector(VECSXP, 2));
				SEXP fieldnames = PROTECT(allocVector(STRSXP, 2));
				SET_VECTOR_ELT(outlist, 0, outfunval);
				SET_STRING_ELT(fieldnames, 0, mkChar("Fun_Value"));
				SET_VECTOR_ELT(outlist, 1, outpoint);
				SET_STRING_ELT(fieldnames, 1, mkChar("Fun_Sensetivity"));
				setAttrib(outlist, R_NamesSymbol, fieldnames);
				UNPROTECT(4);
				return outlist;
			}
		}
	}
	return R_NilValue;
}

bool construct_PSG_POINT_from_robject(SEXP thisRObject, string objname,  CPSGS_Data_Object** thisPSGPoint, string& errorstr)
{ 
	errorstr.clear();
	*thisPSGPoint = NULL;
	char buffererr[128];
	if (!isVector(thisRObject)) {
		sprintf(buffererr, "%s should be a Point(RVector)", objname.c_str());
		errorstr = string(buffererr);
		return false;
	}
	if (!isNumeric(thisRObject)) {
		sprintf(buffererr, "%s should be a Numeric Point(RVector)", objname.c_str());
		errorstr = string(buffererr);
		return false;
	}
	SEXP thisnames = getAttrib(thisRObject, R_NamesSymbol);
	if (isNull(thisnames)) {
		sprintf(buffererr, "%s should has the Variables Names(Columns Names)", objname.c_str());
		errorstr = string(buffererr);
		return false;
	}
	if (!isString(thisnames)) {
		sprintf(buffererr, "%s: Variables Names should be a String", objname.c_str());
		errorstr = string(buffererr);
		return false;
	}
	vector<string> thisvariables_arr;
	const char* thisname;
	for (int iname=0; iname<length(thisnames); iname++) {
		thisname = CHAR(STRING_ELT(thisnames, iname));
		thisvariables_arr.push_back(string(thisname));
	}
	int numelmnt = length(thisRObject);
	bool key_int = false;
	if (isInteger(thisRObject)) {
		key_int = true;
		thisRObject = coerceVector(thisRObject, REALSXP);
	}
	double* thisbody = new double [numelmnt];
	double* pointbody = REAL(thisRObject);
	for (int64_t i=0; i<numelmnt; i++) {
		*(thisbody+i) = *(pointbody+i);
	}
	if (key_int) {
		thisRObject = coerceVector(thisRObject, INTSXP);
	}
	vector<int64_t> size_arr;
	size_arr.resize(2); size_arr[0] = numelmnt; size_arr[1] = 1;
	*thisPSGPoint = new CPSGS_Data_Object(objname, PSGS_POINT, &thisvariables_arr, &size_arr, pointbody);
	delete [] thisbody;
	return true;
}


bool check_ext_functions(vector<EXT_FUNCTION_DESCR_R>* extfunction_arr, SEXP rho, CRPSGSolverHelper* thisImpl, vector<string>& errors_arr)
{
	errors_arr.clear();
	if (extfunction_arr->empty()) return true;
	bool res = true;
	for (size_t i_fun=0; i_fun<extfunction_arr->size(); i_fun++) {
		// check if the function exists
		EXT_FUNCTION_DESCR_R *thisdescr = &(extfunction_arr->at(i_fun));
		SEXP thisfun = PROTECT(findFun( install(thisdescr->fnname.c_str()), rho));
		if (thisfun == R_NilValue) {
			char buffererr[128];
			sprintf(buffererr, "%s not found", thisdescr->fnname.c_str());
			errors_arr.push_back(string(buffererr));
			res = false;
		}
		SEXP thisgrad = PROTECT(findFun( install(thisdescr->grname.c_str()), rho));
		if (thisgrad == R_NilValue) {
			char buffererr[128];
			sprintf(buffererr, "%s not found", thisdescr->grname.c_str());
			errors_arr.push_back(string(buffererr));
			res = false;
		}
		// now look for point to start
		SEXP thispoint = PROTECT(lookforExternelObject(thisdescr->str_fn_args[0], rho));
		if (thispoint==R_UnboundValue) {
			char buffererr[128];
			sprintf(buffererr, "%s not found", thisdescr->str_fn_args[0].c_str());
			errors_arr.push_back(string(buffererr));
			res = false;
		}
		// it must be a point
		CPSGS_Data_Object* thisPSGPoint = NULL;
		string errorstr;
		if (!construct_PSG_POINT_from_robject(thispoint, thisdescr->str_fn_args[0],  &thisPSGPoint, errorstr)) {
			res = false;
			errors_arr.push_back(errorstr);
		}
		if (!res) { // this function failed
			delete thisPSGPoint; thisPSGPoint = NULL;
			UNPROTECT(3); continue;
		}
		else { // OK!
			thisdescr->str_fn_vars = *(thisPSGPoint->m_get_headrarray());
		}
		if (!thisdescr->fnenv) {
			thisdescr->fnenv = rho;
		}
		UNPROTECT(3);
		// try to get function value
		double funres;
		char header[]="\0";
		int numvar = 0;
		int fres = RunRExternalFunction(thisdescr, numvar, header, thisPSGPoint->m_get_data(), thisImpl, &funres);
		if (fres==0) { // this is error
			char buffererr[128];
			sprintf(buffererr, "%s was not evaluated", thisdescr->fnname.c_str());
			errors_arr.push_back(string(buffererr));
			res = false;
		}
		double* grfunres = new double [thisdescr->str_fn_vars.size()];
		fres = RunRGradientExternalFunction(thisdescr, (int)thisdescr->str_fn_vars.size(), header, thisPSGPoint->m_get_data(), thisImpl, &grfunres);
		if (fres==0) { // error
			delete [] grfunres;
			char buffererr[128];
			sprintf(buffererr, "%s was not evaluated", thisdescr->grname.c_str());
			errors_arr.push_back(string(buffererr));
			res = false;
		}
		delete [] grfunres;
		delete thisPSGPoint; thisPSGPoint = NULL;
	}
	return res;
}

SEXP decode_sparse(SEXP input_obj, SEXP rho, int& nrow, int& ncol, vector<string>& colnames, vector<int>& ivec, 
	vector<int>& jvec, vector<double>& xvec, string& errorstr)
{
	nrow = ncol =0; colnames.clear(); ivec.clear(); jvec.clear(); xvec.clear();
	if (input_obj == R_NilValue) {
		errorstr = string("Wrong Input");
		return R_NilValue;
	}
	SEXP whatclass;
	whatclass = getAttrib(input_obj, R_ClassSymbol);
	if (whatclass == R_NilValue) {
		errorstr = string("Cannot get Class Name");
		return R_NilValue;
	}
	if (!isString(whatclass)) {
		errorstr = string("Wrong Class");
		return R_NilValue;
	}
	SEXP convinput_obj;
	if (strcasecmp(CHAR(STRING_ELT(whatclass, 0)), "dgTMatrix") == 0) {
		convinput_obj = input_obj;
	}
	else { // need to convert
		SEXP thisfun = PROTECT(findFun( install("as"), rho));
		if (thisfun == R_NilValue) {
			UNPROTECT(1); 
			errorstr = string("Wrong Input");
			return R_NilValue;
		}
		SEXP RCallBack;
		PROTECT(RCallBack = allocVector(LANGSXP, 3)); // RFun + 2 arguments
		SETCAR(RCallBack, thisfun);
		SETCADR(RCallBack, input_obj);
		SETCADDR(RCallBack, ScalarString(mkChar("dgTMatrix")));
		convinput_obj = eval(RCallBack, rho);
		UNPROTECT(2); 
	}
	SEXP matrdims;
	matrdims = getAttrib(convinput_obj, install("Dim"));
	if (!isVector(matrdims)) {
		errorstr = string("Wrong Dimentions");
		return R_NilValue;
	}
	if (!isInteger(matrdims)) {
		errorstr = string("Dimentions should be integer");
		return R_NilValue;
	}
	nrow = INTEGER(matrdims)[0];
	ncol = INTEGER(matrdims)[1];
	if (nrow==0 || ncol==0) {
		errorstr = string("Empty Sparse Matrix");
		return R_NilValue;
	}
	SEXP thisallnames = getAttrib(convinput_obj, install("Dimnames"));
	if (thisallnames == R_NilValue) {
		errorstr = string("Cannot get Dimnames");
		return R_NilValue;
	}
	SEXP thisnames = VECTOR_ELT(thisallnames, 1);
	for (int i=0; i<ncol; i++)
		colnames.push_back(string(CHAR(STRING_ELT(thisnames, i))));
	SEXP thisi = getAttrib(convinput_obj, install("i"));
	if (thisi == R_NilValue) {
		errorstr = string("Cannot get i");
		return R_NilValue;
	}
	if (!isInteger(thisi)) {
		errorstr = string("i should be integer");
		return R_NilValue;
	}
	int* ibody = INTEGER(thisi);
	for (int i=0; i<length(thisi); i++) 
		ivec.push_back(ibody[i]);
	SEXP thisj = getAttrib(convinput_obj, install("j"));
	if (thisj == R_NilValue) {
		errorstr = string("Cannot get j");
		return R_NilValue;
	}
	if (!isInteger(thisj)) {
		errorstr = string("j should be integer");
		return R_NilValue;
	}
	int* jbody = INTEGER(thisj);
	for (int i=0; i<length(thisj); i++) 
		jvec.push_back(jbody[i]);
	SEXP thisx = getAttrib(convinput_obj, install("x"));
	if (thisx == R_NilValue) {
		errorstr = string("Cannot get x");
		return R_NilValue;
	}
	if (!isReal(thisx)) {
		errorstr = string("x should be real");
		return R_NilValue;
	}
	double* xbody = REAL(thisx);
	for (int i=0; i<length(thisx); i++) 
		xvec.push_back(xbody[i]);
	SEXP out = PROTECT(allocVector(LGLSXP, 1));
	LOGICAL(out)[0] = true;
	UNPROTECT(1);
	return out;
}

extern "C"  SEXP  crpsg_solver(SEXP regime, SEXP path, SEXP thisproblem, SEXP rho, SEXP allowExt, SEXP suppressMessages)
{
	// Initial Loading
	if (!isInteger(regime)) {
		Rprintf("regime must be Integer\n");
		return R_NilValue;
	}
	RPSG_REGIME_TYPE this_rpsg_regime = RPSG_REGIME_UNKNOWN;
	switch (INTEGER(regime)[0]) {
		case 0: this_rpsg_regime = RPSG_REGIME_SOLVE; break;
		case 1: this_rpsg_regime = RPSG_REGIME_VERIFY; break;
		case 2: this_rpsg_regime = RPSG_REGIME_EXPORTTOTEXT; break;
		case 3: this_rpsg_regime = RPSG_REGIME_IMPORTFROMTEXT; break;
	}
	string path_toexport;
	if (this_rpsg_regime == RPSG_REGIME_EXPORTTOTEXT) {
		if (!isString(path)) {
			Rprintf("path must be Character\n");
			return R_NilValue;
		}
		else {
			path_toexport = string(CHAR(STRING_ELT(path, 0)));
			if (path_toexport.empty()) {
				Rprintf("path is empty\n");
				return R_NilValue;
			}
		}
	}
	if (!isLogical(allowExt)) {
		Rprintf("allowExt must be Logical\n");
		return R_NilValue;
	}
	bool key_allowExt = LOGICAL(allowExt)[0];
	if (!isLogical(suppressMessages)) {
		Rprintf("suppressMessages must be Logical\n");
		return R_NilValue;
	}
	bool key_suppressMessages = LOGICAL(suppressMessages)[0];
	if (!isEnvironment(rho)) {
		Rprintf("Environment not found\n");
		return R_NilValue;
	}
	if (!isNewList(thisproblem)) {
		Rprintf("Argument should be a list\n");
		return R_NilValue;
	}
	int num_fields = length(thisproblem); // length of input
	if (num_fields==0) {
		Rprintf("Input list is empty\n");
		return R_NilValue;
	}
	SEXP names = getAttrib(thisproblem, R_NamesSymbol);
	if (names==R_NilValue) {
		Rprintf("Input list is empty\n");
		return R_NilValue;
	}
	CPSGS_Problem* this_problem = new CPSGS_Problem();
	vector<string> error_str_arr;
	if (!encode_RPSG_Problem(thisproblem, this_problem, rho, error_str_arr)) {
		delete this_problem;
		for (size_t i=0; i<error_str_arr.size(); i++) {
			Rprintf("%s\n", error_str_arr[i].c_str());
		}
		Rprintf("Problem cannot be solved\n");
		return R_NilValue;
	}
	// create CRPSGSolverHelper
	CRPSGSolverHelper* thisCRPSGSolverHelper = new CRPSGSolverHelper();
	if (!thisCRPSGSolverHelper->m_set_R_Functions(&PrintToR, &ProcessStopToR, &CycleFinishToR, &ProcessCancelToR,
			&RunRExternalFunction, &RunRGradientExternalFunction,
			&RunRExternalFunctionDir, &RunRGradientExternalFunctionDir)) {
		Rprintf("Implicit Functions was not set\n");
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	thisCRPSGSolverHelper->m_set_suppressMessages(key_suppressMessages);
	thisCRPSGSolverHelper->m_set_problem(this_problem);
	char pBuffer[PATH_MAX];
	memset(pBuffer, 0, PATH_MAX);
	if (getcwd(pBuffer, PATH_MAX)==NULL) {
		thisCRPSGSolverHelper->OnError(-1, "PSG Loader", "Cannot find current folder");
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	this_problem->m_set_root_path(string(pBuffer));
	if (this_problem->m_get_statement().empty()) {
		thisCRPSGSolverHelper->OnError(-1, "PSG Loader", "Problem statement is empty");
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	// collect external functions
	vector<string> errors_arr;
	int numextfun = thisCRPSGSolverHelper->CollectExternalFunctions(errors_arr);
	if (numextfun<0) {
		for (size_t i=0; i<errors_arr.size(); i++) {
			thisCRPSGSolverHelper->OnError(-1, "PSG Loader(External Functions)", errors_arr[i].c_str());
		}
		char buffererr[128];
		sprintf(buffererr, "%s is not ready", this_problem->m_get_name().c_str());
		thisCRPSGSolverHelper->OnError(-1, "PSG Loader", buffererr);
		delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
		return R_NilValue;
	}
	// check external functions
	if (numextfun>0) {
		vector<EXT_FUNCTION_DESCR_R>* thisExtFuncDescr_arr = thisCRPSGSolverHelper->m_get_extfunction_arr();
		if (!check_ext_functions(thisExtFuncDescr_arr, rho, thisCRPSGSolverHelper, errors_arr)) {
			for (size_t i=0; i<errors_arr.size(); i++) {
				thisCRPSGSolverHelper->OnError(-1, "PSG Loader(External Functions)", errors_arr[i].c_str());
			}
			char buffererr[128];
			sprintf(buffererr, "%s is not ready", this_problem->m_get_name().c_str());
			thisCRPSGSolverHelper->OnError(-1, "PSG Loader", buffererr);
			delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
			return R_NilValue;
		}
	}
	// check if the problem ready
	if (!thisCRPSGSolverHelper->IsProblemReadyToStart()) {
	}
	vector<string> lost_obj_arr;
	if (!thisCRPSGSolverHelper->IsProblemHasAllInputData(lost_obj_arr)) {
		bool key_OK = false;
		size_t num_fieds_in_addlist = lost_obj_arr.size();
		if (key_allowExt && (num_fieds_in_addlist!=0)) { // try to find External
			error_str_arr.clear();
			string errorstrloc;
			key_OK = true;
			SEXP addinplist = PROTECT(allocVector(VECSXP, num_fieds_in_addlist));
			SEXP addfieldnames = PROTECT(allocVector(STRSXP, num_fieds_in_addlist));
			for (size_t i=0; i<num_fieds_in_addlist; i++) {
				SEXP rextobj =  PROTECT(lookforExternelObjectExt(lost_obj_arr[i], this_problem->m_get_statement(), rho, errorstrloc));
				if (rextobj == R_UnboundValue) {
					error_str_arr.push_back(errorstrloc);
					UNPROTECT(1);
					key_OK = false;
					continue;
				}
				else {
					SET_VECTOR_ELT(addinplist, i, rextobj);
					SET_STRING_ELT(addfieldnames, i, mkChar(lost_obj_arr[i].c_str()));
					UNPROTECT(1);
				}
			}
			setAttrib(addinplist, R_NamesSymbol, addfieldnames);
			if (key_OK) { // add objects
				error_str_arr.clear();
				key_OK = encode_RPSG_Problem(addinplist, this_problem, rho, error_str_arr);
			}
			UNPROTECT(2);
		}
		else {
			error_str_arr = lost_obj_arr;
		}
		if (!key_OK) { // not all found
			char buffererr[128];
			sprintf(buffererr, "Problem is not ready");
			thisCRPSGSolverHelper->OnError(-1, "PSG Loader", buffererr);
			for (size_t i=0; i<error_str_arr.size(); i++) {
				sprintf(buffererr, "%s", error_str_arr[i].c_str());
				thisCRPSGSolverHelper->OnError(-1, "PSG Loader", buffererr);
			}
			delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
			return R_NilValue;
		}
		else {
			for (size_t i=0; i<lost_obj_arr.size(); i++) {
				thisCRPSGSolverHelper->OnWarning(-1, lost_obj_arr[i].c_str(), "Found in External Env");
			}
		}
	}
	// Initial Loading
	// Data gotten! Solve the problem
//	}
	if ((this_rpsg_regime==RPSG_REGIME_SOLVE)||(this_rpsg_regime==RPSG_REGIME_VERIFY)) {
		int sRet;
		if (this_rpsg_regime==RPSG_REGIME_SOLVE) 
			sRet = CRPSGSolverHelper::run_rpsg_solver(thisCRPSGSolverHelper);
		else if (this_rpsg_regime==RPSG_REGIME_VERIFY) 
			sRet = CRPSGSolverHelper::run_rpsg_verify(thisCRPSGSolverHelper);
		if (sRet == -9999) {
			thisCRPSGSolverHelper->OnError(0, "PSG Solver", "PSG license is expired");
		}
		else if (sRet>0) {
			if (thisCRPSGSolverHelper->m_get_whattodo() != HELPER_REGIME_INTERRUPT_R) {
				thisCRPSGSolverHelper->OnError(0, "PSG Solver", "Error while Problem Statement processing");
			}
		}
	}
	else if (this_rpsg_regime==RPSG_REGIME_EXPORTTOTEXT) {// Export To Text
		// correct path
		string errorstr;
		string locproblem_name = this_problem->m_get_name();
		if (!check_and_correct_pathimportexport(path_toexport, false, locproblem_name, errorstr)) {
			thisCRPSGSolverHelper->OnError(0, "Export PSG to Text", errorstr.c_str());
			Rprintf("%s was not exported\n", thisCRPSGSolverHelper->m_get_problem()->m_get_name().c_str());
			string errorstr;
			SEXP outres = construct_logical(false, errorstr);
			return outres;
		}
		if (locproblem_name != this_problem->m_get_name()) {
			this_problem->m_set_name(locproblem_name);
		}
		int res = CRPSGSolverHelper::ExportProblemToTextFiles(path_toexport, thisCRPSGSolverHelper);
		if (res<=0) {
			char buffererr[128];
			sprintf(buffererr, "%s was not exported", thisCRPSGSolverHelper->m_get_problem()->m_getlasterror().c_str());
			thisCRPSGSolverHelper->OnError(0, "Export PSG to Text", buffererr);
			Rprintf("%s was not exported\n", thisCRPSGSolverHelper->m_get_problem()->m_get_name().c_str());
			string errorstr;
			SEXP outres = construct_logical(false, errorstr);
			return outres;
		}
		else {
			Rprintf("%s was exported\n", thisCRPSGSolverHelper->m_get_problem()->m_get_name().c_str());
			string errorstr;
			SEXP outres = construct_logical(true, errorstr);
			return outres;
		}
	}
	vector<string> solver_errors_arr;
	bool key_solver_errors = thisCRPSGSolverHelper->m_get_problem()->m_get_output_errors(solver_errors_arr);
	if (this_rpsg_regime==RPSG_REGIME_VERIFY) { // Verify Exit
		if (key_solver_errors) { //
			string errorstr;
			Rprintf("Verification met errors.\nSee Errors in Output\n");
			delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
			SEXP outerrors = construct_string_array(&solver_errors_arr, errorstr);
			return outerrors;
		}
		else {
			string errorstr;
			Rprintf("Solver finished verification.\nIt's OK.");
			delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
			SEXP outerrors = construct_string(string("Verified!"), errorstr);
			return outerrors;
		}
	}
	// OK, get solution
	else if (this_rpsg_regime==RPSG_REGIME_SOLVE) {
		SEXP outlist = R_NilValue;
		if (thisCRPSGSolverHelper->m_get_whattodo()==HELPER_REGIME_SOLVED_R) { // OK, Collect Output
			CPSGS_Problem* this_finproblem = thisCRPSGSolverHelper->m_get_problem();
			if (this_finproblem->m_is_problem_regular()) {
				CPSGS_OutputProblem* this_out_problem = this_finproblem->m_get_curr_output_problem();
				bool res;
				outlist = construct_PSG_Problem_Output(this_out_problem, error_str_arr, res);
				if (!res) {
					char buffererr[128];
					for (size_t i=0; i<error_str_arr.size(); i++) {
						sprintf(buffererr, "%s\n", error_str_arr[i].c_str());
						thisCRPSGSolverHelper->OnError(0, "PSG Loader", buffererr);
					}
					delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
					return R_NilValue;
				}
				else {
					Rprintf("OK. Solver Finished\n\n");
					delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
					return outlist;
				}
			}
			else if (this_finproblem->m_is_problem_cyclic()) {
				bool key_glob = true;
				vector<CPSGS_OutputProblem*>* output_problem_collection =  this_finproblem->m_get_output_problem_collection();
				size_t num_out_problems = output_problem_collection->size();
				if (num_out_problems != 0) {
					outlist = allocVector(VECSXP, num_out_problems);
				}
				for (size_t i_prob = 0; i_prob < num_out_problems; i_prob++) {
					bool res = true;
					SEXP outlistloc = construct_PSG_Problem_Output(output_problem_collection->at(i_prob), error_str_arr, res);
					if ((outlistloc==R_NilValue) || !res) {
						SET_VECTOR_ELT(outlist, i_prob, R_NilValue);
						key_glob = false;
						if (!res) {
							char buffererr[128];
							for (size_t i=0; i<error_str_arr.size(); i++) {
								sprintf(buffererr, "%s\n", error_str_arr[i].c_str());
								thisCRPSGSolverHelper->OnError(0, "PSG Loader", buffererr);
							}
						}
					}
					else {
						SET_VECTOR_ELT(outlist, i_prob, outlistloc);
					}
				}
				delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
				if (key_glob) {
					Rprintf("OK. Solution Filled\n\n");
					return outlist;
				}
				else {
					return R_NilValue;
				}
			}
		}
		else if (thisCRPSGSolverHelper->m_get_whattodo()==HELPER_REGIME_INTERRUPT_R) {
		}
	}
	Rprintf("NAN. Solver Error\n\n");
	delete thisCRPSGSolverHelper; thisCRPSGSolverHelper = NULL;
	return R_NilValue;
}
