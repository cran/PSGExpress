#include "PSGShellObjects.h"
#include "../../PSGSolver/src/PSGSolver.h"
#include "../../IPSGSolverHelper/src/IPSGSolverHelper.h"
#include <map>
#include <cassert>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <fstream>
#include <iostream>
#include <math.h>
#include <time.h> 
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <vector>

using namespace std;

typedef struct  {
	int objecttype;
	long namelen;
	long headerlen;
	int64_t mrows;
	int64_t ncols;
	int64_t fullsize;
} object_description;

typedef struct  {
	long namelen;
	long problemstatementlen;
	long outputlen;
	long logstrlen;
	long errorstrlen;
	long outputobjnum;
	int64_t fullsize;
} output_description;


typedef struct  {
	long namelen;
	long problemstatementlen;
	time_t storetime;
	long version;
	long platform;
	long inputobjnum;
	long outputobjnum;
	long keyprocessed;
	int64_t fullsize;
} problem_description;

#ifdef WIN32
	char LPRF[]="x%ld";
#elif LINUX
	char LPRF[]="x%jd";
#else
#error Platform not supported
#endif

void trimstr(string& str)
{
	string::size_type pos1 = str.find_first_not_of(" \r\n\t");
	string::size_type pos2 = str.find_last_not_of(" \r\n\t");
	str = str.substr(pos1 == string::npos ? 0 : pos1, 
		pos2 == string::npos ? str.length() - 1 : pos2 - pos1 + 1);
}

// Some utils
namespace strutil_shell {
    using namespace std;
#ifdef LINUX
    int __attribute__ ((visibility("hidden"))) mytolower(int c) {return tolower(c);}
    int __attribute__ ((visibility("hidden"))) mytoupper(int c) {return toupper(c);}
#else
    int mytolower(int c) {return tolower(c);}
    int mytoupper(int c) {return toupper(c);}
#endif

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
    string __attribute__ ((visibility("hidden"))) toLower(const string& str)
#else
	string toLower(const string& str)
#endif
    {
        string t = str;
        transform(t.begin(), t.end(), t.begin(), mytolower);
        return t;
    }

#ifdef LINUX
    string __attribute__ ((visibility("hidden"))) toUpper(const string& str)
#else
	string toUpper(const string& str)
#endif
	{
        string t = str;
        transform(t.begin(), t.end(), t.begin(), mytoupper);
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
	int __attribute__ ((visibility("hidden"))) istherestringinvector(const string& strinp, const vector<string>& strarr)
#else
	int istherestringinvector(const string& strinp, const vector<string>& strarr)
#endif
	{
	int res = -1;
	string str = toLower(strinp);
	for (size_t i=0; i<strarr.size(); i++) {
		string loc = toLower(strarr[i]);
		if (loc.find(str) == 0)	{
			return i;
		}
	}
	return res;
	}


#ifdef LINUX
	   char __attribute__ ((visibility("hidden"))) *chartrim(char *buffer, char *stripchars)
#else
	char *chartrim(char *buffer, char *stripchars)
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
// Some utils

int load_problem_statement_from_text(const std::string pathtofile, std::string& statement, std::string& problemname, std::string& errorstr)
{
	statement.clear(); errorstr.clear();
	ifstream problemfile;
	problemfile.open((const char*)pathtofile.c_str(), ios_base::in);
	if(problemfile.fail())	{
		errorstr = "Cannot open "+pathtofile;
		return -1;
	}
	problemfile.seekg(0, ios::beg);
	string tmpBuff;
	while(getline(problemfile, tmpBuff)) {
		statement += tmpBuff+string("\n");
	}
	problemfile.close();
	if (problemname.empty() && (!statement.empty())) {
		vector<string> str_arr;
		str_arr = strutil_shell::split(statement, string(" ()*-=,\t\r\n;[]"), false);
		int numstr = strutil_shell::istherestringinvector(string("problem_"), str_arr);
		if (numstr>=0) {
			problemname = str_arr[numstr];
		}
	}
	return (int)statement.size();
}

int get_PSG_objects_from_arr(vector<string>* inp_str_arr, vector<string>& point_arr, vector<string>& vector_arr, vector<string>& matrix_arr, vector<string>& pmatrix_arr, vector<string>& added_objects_arr)
{
	if (inp_str_arr->size() == 0) return 0;
	vector<string> objects_to_exclude;
	vector<string> patterns_arr;
	patterns_arr.push_back("point_");
	patterns_arr.push_back("vector_");
	patterns_arr.push_back("matrix_");
	patterns_arr.push_back("pmatrix_");
	vector<string>::iterator it_addobjstr = find(inp_str_arr->begin(), inp_str_arr->end(), string("addedobjects"));
	if (it_addobjstr != inp_str_arr->end()) { // there are added objects
		for (vector<string>::iterator it_str=it_addobjstr; it_str!=inp_str_arr->end(); it_str++) {
			for (size_t i=0; i<patterns_arr.size(); i++) {
				if (strutil_shell::toLower(*it_str).find(patterns_arr[i])==0) {
					if (find(added_objects_arr.begin(), added_objects_arr.end(), *it_str)==added_objects_arr.end()) {
						added_objects_arr.push_back(*it_str);
					}
					break;
				}
			}
		}
	}
	for (vector<string>::iterator it_str=inp_str_arr->begin(); it_str!=it_addobjstr; it_str++) {
		if (it_str->find(patterns_arr[0])==0) { // point
				if (find(point_arr.begin(), point_arr.end(), *it_str)==point_arr.end()) { // 
					point_arr.push_back(*it_str);
				}
		}
		else if (it_str->find(patterns_arr[1])==0) { // vector
				if (find(vector_arr.begin(), vector_arr.end(), *it_str)==vector_arr.end()) { // 
					vector_arr.push_back(*it_str);
				}
		}
		else if (it_str->find(patterns_arr[2])==0) { // matrix
				if (find(matrix_arr.begin(), matrix_arr.end(), *it_str)==matrix_arr.end()) { // 
					matrix_arr.push_back(*it_str);
				}
		}
		else if (it_str->find(patterns_arr[3])==0) { // pmatrix
				if (find(pmatrix_arr.begin(), pmatrix_arr.end(), *it_str)==pmatrix_arr.end()) { // 
					pmatrix_arr.push_back(*it_str);
				}
		}
	}
	return 1;
}

int read_matrix_fromtext_new(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int& n_rows, int& n_cols, std::string& errorstr)
{
	ifstream objectfile;
	errorstr.clear();
	*matrix_body = NULL;
	header_arr.clear();
	n_rows = n_cols = 0;
	objectfile.open((const char*)pathtoobjfile.c_str(), ios_base::in);
	if(objectfile.fail())	{
		errorstr = string("Cannot open ")+pathtoobjfile;
		return -1;
	}
	objectfile.seekg(0, ios::beg);
	string tmpBuff;
	tmpBuff.clear();
	while(getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()!=0) break;
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("matrix is empty");
		return -1;
	}
	char *buffer;
	char seps[]   = " \t;";
	char *token, *next_token;
	size_t strlen;
	int64_t numcols(0);
	int64_t numrows(0);
	vector<double*> pdata_arr;
	if (isalpha(tmpBuff.c_str()[0])) {
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		while (token != NULL)	{
			header_arr.push_back(string(token));
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		}
		delete [] buffer;
		numcols = header_arr.size();
		while (getline(objectfile, tmpBuff)) {
			trimstr(tmpBuff);
			if (tmpBuff.size()!=0) break;
		}
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("matrix is empty");
		return -1;
	}
	strlen = tmpBuff.size()+1;
	buffer = (char*) malloc (strlen * sizeof(char));
	memset(buffer, 0, sizeof(char) * strlen);
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	int64_t count_col(0);
	if (numcols==0) {
		while (token != NULL)	{
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		}
		numcols = count_col;
	}
	double *dataloc;
	dataloc = new double[numcols];
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	count_col = 0;
	while (token != NULL)	{
		*(dataloc+count_col) = atof(token);
		count_col++;
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		if (count_col>=numcols) break;
	}
	delete [] buffer;
	if ((count_col<numcols) || (token != NULL)) {
		delete [] dataloc;
		objectfile.close();
		errorstr = string("wrong matrix format");
		return -1;
	}
	pdata_arr.push_back(dataloc);
	while (getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()==0) continue;
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		count_col = 0;
		dataloc = new double[numcols];
		while (token != NULL)	{
			*(dataloc+count_col) = atof(token);
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
			if (count_col>=numcols) break;
		}
		delete [] buffer;
		if ((count_col<numcols) || (token != NULL)) {
			delete [] dataloc;
			for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
				delete [] *it_row;
			}
			objectfile.close();
			errorstr = string("wrong matrix format");
			return -1;
		}
		pdata_arr.push_back(dataloc);
	}
	numrows = pdata_arr.size();
	*matrix_body = new double[numrows*numcols];
	double* pdata = *matrix_body;
	for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
		for (int64_t ncol=0; ncol<numcols; ncol++) {
			(*pdata++) = *(*it_row+ncol);
		}
		delete [] *it_row;
	}
	objectfile.close();
	n_rows = numrows; n_cols = numcols;
	if (header_arr.size()==0) {
		char buffloc[128];
		for (long i=0; i<numcols; i++){
			sprintf(buffloc, "x%ld", i+1);
			header_arr.push_back(string(buffloc));
		}
	}
	return 1;
}

int read_matrix_fromtext_parse(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, double** benchmark_body, double** probability_body, int64_t& n_rows, int64_t& n_cols, std::string& errorstr)
{
	ifstream objectfile;
	errorstr.clear();
	*matrix_body = NULL;
	header_arr.clear();
	n_rows = n_cols = 0;
	objectfile.open((const char*)pathtoobjfile.c_str(), ios_base::in);
	if(objectfile.fail())	{
		errorstr = string("Cannot open ")+pathtoobjfile;
		return -1;
	}
	objectfile.seekg(0, ios::beg);
	string tmpBuff;
	tmpBuff.clear();
	while(getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()!=0) break;
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("matrix is empty");
		return -1;
	}
	char *buffer;
	char seps[]   = " \t;";
	char *token, *next_token;
	size_t strlen;
	int64_t numcols(0);
	int64_t numrows(0);
	int64_t posid(-1);
	int64_t posbenchmark(-1);
	int64_t posprobability(-1);
	vector<double> probability_vec; probability_vec.resize(0);
	vector<double> benchmark_vec; benchmark_vec.resize(0);
	vector<double*> pdata_arr;
	if (isalpha(tmpBuff.c_str()[0])) {
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		numcols = 0;
		while (token != NULL)	{
			numcols++;
			if (strcasecmp(token, "id")==0) {
				posid = numcols-1;
				token = strutil_shell::strtok_uni(NULL, seps, &next_token);
				continue;
			}
			else if (strcasecmp(token, "scenario_benchmark")==0) {
				posbenchmark = numcols-1;
				token = strutil_shell::strtok_uni(NULL, seps, &next_token);
				continue;
			}
			else if (strcasecmp(token, "scenario_probability")==0) {
				posprobability = numcols-1;
				token = strutil_shell::strtok_uni(NULL, seps, &next_token);
				continue;
			}
			header_arr.push_back(string(token));
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		}
		delete [] buffer;
		while (getline(objectfile, tmpBuff)) {
			trimstr(tmpBuff);
			if (tmpBuff.size()!=0) break;
		}
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("matrix is empty");
		return -1;
	}
	strlen = tmpBuff.size()+1;
	buffer = (char*) malloc (strlen * sizeof(char));
	memset(buffer, 0, sizeof(char) * strlen);
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	int64_t count_col(0);
	if (numcols==0) {
		while (token != NULL)	{
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		}
		numcols = count_col;
	}
	double *dataloc;
	dataloc = new double[numcols];
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	count_col = 0;
	int count_data = 0;
	while (token != NULL)	{
		if ((count_col!=posid) && (count_col!=posbenchmark) && (count_col!=posprobability)) {
			*(dataloc+count_data) = atof(token); count_data++;
		}
		else {
			if (count_col==posbenchmark) benchmark_vec.push_back(atof(token));
			else if (count_col==posprobability) probability_vec.push_back(atof(token));
		}
		count_col++;
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		if (count_col>=numcols) break;
	}
	delete [] buffer;
	if ((count_col<numcols) || (token != NULL)) {
		delete [] dataloc;
		objectfile.close();
		errorstr = string("wrong matrix format");
		return -1;
	}
	pdata_arr.push_back(dataloc);
	while (getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()==0) continue;
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		count_col = 0;
		count_data = 0;
		dataloc = new double[numcols];
		while (token != NULL)	{
			if ((count_col!=posid) && (count_col!=posbenchmark) && (count_col!=posprobability)) {
				*(dataloc+count_data) = atof(token); count_data++;
			}
			else {
				if (count_col==posbenchmark) benchmark_vec.push_back(atof(token));
				else if (count_col==posprobability) probability_vec.push_back(atof(token));
			}
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
			if (count_col>=numcols) break;
		}
		delete [] buffer;
		if ((count_col<numcols) || (token != NULL)) {
			delete [] dataloc;
			for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
				delete [] *it_row;
			}
			objectfile.close();
			errorstr = string("wrong matrix format");
			return -1;
		}
		pdata_arr.push_back(dataloc);
	}
	numrows = pdata_arr.size();
	double* pdata = NULL;
	*benchmark_body  = *probability_body = NULL;
	if (!probability_vec.empty()) {
		if ((int64_t)probability_vec.size() != numrows) {
			errorstr = string("wrong probability size");
			return -1;
		}
		*probability_body = new double[probability_vec.size()];
		pdata = *probability_body;
		numcols--;
		for (size_t i=0; i < probability_vec.size(); i++)
			(*pdata++) = probability_vec.at(i);
	}
	if (!benchmark_vec.empty()) {
		if ((int64_t)benchmark_vec.size() != numrows) {
			errorstr = string("wrong benchmark size");
			delete [] *probability_body;
			return -1;
		}
		*benchmark_body  = new double[numrows];
		pdata = *benchmark_body;
		numcols--;
		for (size_t i=0; i < benchmark_vec.size(); i++)
			(*pdata++) = benchmark_vec.at(i);
	}
	if (posid>=0) numcols--;
	*matrix_body = new double[numrows*numcols];
	pdata = *matrix_body;
	for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
		for (int64_t ncol=0; ncol<numcols; ncol++) {
			(*pdata++) = *(*it_row+ncol);
		}
		delete [] *it_row;
	}
	objectfile.close();
	n_rows = numrows; n_cols = numcols;
	if (header_arr.size()==0) { // default header
		char buffloc[128];
		for (int64_t i=0; i<numcols; i++){
			sprintf(buffloc, LPRF, i+1);
			header_arr.push_back(string(buffloc));
		}
	}
	return 1;
}

int read_matrix_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, int64_t& n_cols, std::string& errorstr)
{
	ifstream objectfile;
	errorstr.clear();
	*matrix_body = NULL;
	header_arr.clear();
	n_rows = n_cols = 0;
	objectfile.open((const char*)pathtoobjfile.c_str(), ios_base::in);
	if(objectfile.fail())	{
		errorstr = string("Cannot open ")+pathtoobjfile;
		return -1;
	}
	objectfile.seekg(0, ios::beg);
	string tmpBuff;
	tmpBuff.clear();
	while(getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()!=0) break;
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("matrix is empty");
		return -1;
	}
	char *buffer;
	char seps[]   = " \t;";
	char *token, *next_token;
	size_t strlen;
	int64_t numcols(0);
	int64_t numrows(0);
	vector<double*> pdata_arr;
	if (isalpha(tmpBuff.c_str()[0])) { // thius is the header
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		while (token != NULL)	{
			header_arr.push_back(string(token));
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		}
		delete [] buffer;
		numcols = header_arr.size();
		while (getline(objectfile, tmpBuff)) {
			trimstr(tmpBuff);
			if (tmpBuff.size()!=0) break;
		}
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("matrix is empty");
		return -1;
	}
	strlen = tmpBuff.size()+1;
	buffer = (char*) malloc (strlen * sizeof(char));
	memset(buffer, 0, sizeof(char) * strlen);
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	int64_t count_col(0);
	if (numcols==0) {
		while (token != NULL)	{
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		}
		numcols = count_col;
	}
	double *dataloc;
	dataloc = new double[numcols];
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	count_col = 0;
	while (token != NULL)	{
		*(dataloc+count_col) = atof(token);
		count_col++;
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		if (count_col>=numcols) break;
	}
	delete [] buffer;
	if ((count_col<numcols) || (token != NULL)) {
		delete [] dataloc;
		objectfile.close();
		errorstr = string("wrong matrix format");
		return -1;
	}
	pdata_arr.push_back(dataloc);
	while (getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()==0) continue;
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		count_col = 0;
		dataloc = new double[numcols];
		while (token != NULL)	{
			*(dataloc+count_col) = atof(token);
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
			if (count_col>=numcols) break;
		}
		delete [] buffer;
		if ((count_col<numcols) || (token != NULL)) {
			delete [] dataloc;
			for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
				delete [] *it_row;
			}
			objectfile.close();
			errorstr = string("wrong matrix format");
			return -1;
		}
		pdata_arr.push_back(dataloc);
	}
	numrows = pdata_arr.size();
	*matrix_body = new double[numrows*numcols];
	double* pdata = *matrix_body;
	for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
		for (int64_t ncol=0; ncol<numcols; ncol++) {
			(*pdata++) = *(*it_row+ncol);
		}
		delete [] *it_row;
	}
	objectfile.close();
	n_rows = numrows; n_cols = numcols;
	if (header_arr.size()==0) {
		char buffloc[128];
		for (int64_t i=0; i<numcols; i++){
			sprintf(buffloc, LPRF, i+1);
			header_arr.push_back(string(buffloc));
		}
	}
	return 1;
}

int read_pmatrix_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, int64_t& n_cols, int64_t& n_nz, std::string& errorstr)
{
	ifstream objectfile;
	errorstr.clear();
	*matrix_body = NULL;
	header_arr.clear();
	n_rows = n_cols = 0;
	objectfile.open((const char*)pathtoobjfile.c_str(), ios_base::in);
	if(objectfile.fail())	{
		errorstr = string("Cannot open ")+pathtoobjfile;
		return -1;
	}
	objectfile.seekg(0, ios::beg);
	string tmpBuff;
	tmpBuff.clear();
	while(getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()!=0) break;
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("matrix is empty");
		return -1;
	}
	char *buffer;
	char seps[]   = " \t;";
	char *token, *next_token;
	size_t strlen;
	int64_t numcols(0);
	int64_t numrows(0);
	int64_t nnz(0);
	vector<double*> pdata_arr;
	if (isalpha(tmpBuff.c_str()[0])) {
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		while (token != NULL)	{
			header_arr.push_back(string(token));
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		}
		delete [] buffer;
		numcols = header_arr.size();
		while (getline(objectfile, tmpBuff)) {
			trimstr(tmpBuff);
			if (tmpBuff.size()!=0) break;
		}
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("matrix is empty");
		return -1;
	}
	double fnumcols=0.0;
	double fnumrows=0.0;
	strlen = tmpBuff.size()+1;
	buffer = (char*) malloc (strlen * sizeof(char));
	memset(buffer, 0, sizeof(char) * strlen);
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	int64_t count_col = 0;
	double *dataloc;
	dataloc = new double[3];
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	count_col = 0;
	while (token != NULL)	{
		*(dataloc+count_col) = atof(token);
		count_col++;
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		if (count_col>=3) break;
	}
	delete [] buffer;
	if ((count_col<3) || (token != NULL)) {
		delete [] dataloc;
		objectfile.close();
		errorstr = string("wrong matrix format");
		return -1;
	}
	fnumrows = max(fnumrows, dataloc[0]);
	fnumcols = max(fnumcols, dataloc[1]);
	pdata_arr.push_back(dataloc);
	while (getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()==0) continue;
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		count_col = 0;
		dataloc = new double[3];
		while (token != NULL)	{
			*(dataloc+count_col) = atof(token);
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
			if (count_col>=3) break;
		}
		delete [] buffer;
		if ((count_col<3) || (token != NULL)) {
			delete [] dataloc;
			for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
				delete [] *it_row;
			}
			objectfile.close();
			errorstr = string("wrong matrix format");
			return -1;
		}
		fnumrows = max(fnumrows, dataloc[0]);
		fnumcols = max(fnumcols, dataloc[1]);
		pdata_arr.push_back(dataloc);
	}
	numrows = (int64_t)fnumrows;
	numcols = max(numcols, (int64_t)fnumcols);
	nnz = (int64_t)pdata_arr.size();
	*matrix_body = new double[nnz*3];
	double* prowdata = *matrix_body;
	double* pcoldata = *matrix_body+1;
	double* pdata = *matrix_body+2;
	for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
		*prowdata = *(*it_row); prowdata+=3;
		*pcoldata = *((*it_row)+1); pcoldata+=3;
		*pdata = *((*it_row)+2); pdata+=3;
		delete [] *it_row;
	}
	objectfile.close();
	n_rows = numrows; n_cols = numcols; n_nz = nnz;
	if (header_arr.size()==0) { // default header
		char buffloc[128];
		for (int64_t i=0; i<numcols; i++){
			sprintf(buffloc, LPRF, i+1);
			header_arr.push_back(string(buffloc));
		}
	}
	return 1;
}

int read_vector_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, std::string& errorstr)
{
	ifstream objectfile;
	errorstr.clear();
	*matrix_body = NULL;
	header_arr.clear();
	n_rows = 0;
	objectfile.open((const char*)pathtoobjfile.c_str(), ios_base::in);
	if(objectfile.fail())	{
		errorstr = string("Cannot open ")+pathtoobjfile;
		return -1;
	}
	objectfile.seekg(0, ios::beg);
	string tmpBuff;
	tmpBuff.clear();
	while(getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()!=0) break;
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("vector is empty");
		return -1;
	}
	char *buffer;
	char seps[]   = " \t;";
	char *token, *next_token;
	size_t strlen;
	int64_t numcols(0);
	int64_t numrows(0);
	const char InfPos[] = "INFINITY";
	const char InfNeg[] = "-INFINITY";
	vector<double*> pdata_arr;
	if (isalpha(tmpBuff.c_str()[0])) { // thius is the header
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		while (token != NULL)	{
			header_arr.push_back(string(token));
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		}
		delete [] buffer;
		numcols = header_arr.size();
		bool key_OK = true;
		if (numcols>2) {
			key_OK = false;
		}
		else {
			switch (numcols) {
				case 1:
				if (strcasecmp(header_arr[0].c_str(), "value") != 0) {key_OK = false;} break;
				case 2:
				if ((strcasecmp(header_arr[0].c_str(), "id")!=0)||(strcasecmp(header_arr[1].c_str(), "value")!=0)) {key_OK = false;} break;
			}
		}
		if (!key_OK) {
			header_arr.clear();
			objectfile.close();
			errorstr = string("wrong header");
			return -1;
		}
		while (getline(objectfile, tmpBuff)) {
			trimstr(tmpBuff);
			if (tmpBuff.size()!=0) break;
		}
	}
	if (tmpBuff.size()==0) {
		header_arr.clear();
		objectfile.close();
		errorstr = string("vector is empty");
		return -1;
	}
	strlen = tmpBuff.size()+1;
	buffer = (char*) malloc (strlen * sizeof(char));
	memset(buffer, 0, sizeof(char) * strlen);
	strcpy(buffer, tmpBuff.c_str());
	int64_t count_col = 0;
	if (numcols==0) { // check num cols
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		while (token != NULL)	{
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
			if (count_col>=2) break;
		}
		if (token != NULL) {
			delete [] buffer;
			objectfile.close();
			errorstr = string("wrong vector format");
			return -1;
		}
		numcols = count_col;
	}
	double *dataloc;
	dataloc = new double[2];
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	count_col = 0;
	if (numcols == 1) {
		*dataloc = (double)(++numrows);
		count_col++;
	}
	else {
		++numrows;
	}
	while (token != NULL)	{
		if (isalpha(token[0])) {
			if (strcasecmp(token, InfPos)==0) {
				*(dataloc+count_col) = INFINITY_POS;
			}
			else if (strcasecmp(token, InfNeg)==0) {
				*(dataloc+count_col) = INFINITY_NEG;
			}
			else {
				*(dataloc+count_col) = atof(token);
			}
		}
		else {
			*(dataloc+count_col) = atof(token);
		}
		count_col++;
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		if (count_col>=2) break;
	}
	delete [] buffer;
	if ((count_col != 2) || (token != NULL)) {
		delete [] dataloc;
		objectfile.close();
		errorstr = string("wrong vector format");
		return -1;
	}
	pdata_arr.push_back(dataloc);
	while (getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()==0) continue;
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		count_col = 0;
		dataloc = new double[2];
		if (numcols == 1) {
			*dataloc = (double)(++numrows);
			count_col++;
		}
		else {
			++numrows;
		}
		while (token != NULL)	{
			*(dataloc+count_col) = atof(token);
			count_col++;
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
			if (count_col>=2) break;
		}
		delete [] buffer;
		if ((count_col != 2) || (token != NULL)) {
			delete [] dataloc;
			for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
				delete [] *it_row;
			}
			objectfile.close();
			errorstr = string("wrong vector format");
			return -1;
		}
		pdata_arr.push_back(dataloc);
	}
	*matrix_body = new double[numrows*2];
	double* piddata = *matrix_body;
	double* pdata = *matrix_body+1;
	for (vector<double*>::iterator it_row=pdata_arr.begin(); it_row!=pdata_arr.end(); it_row++) {
		*piddata = *(*it_row); piddata+=2;
		*pdata = *((*it_row)+1); pdata+=2;
		delete [] *it_row;
	}
	objectfile.close();
	n_rows = numrows; 
	if (header_arr.size()==0) {
		header_arr.push_back(string("id"));
		header_arr.push_back(string("value"));
	}
	return 1;
}

int read_point_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, std::string& errorstr)
{
	ifstream objectfile;
	errorstr.clear();
	*matrix_body = NULL;
	header_arr.clear();
	n_rows = 0;
	objectfile.open((const char*)pathtoobjfile.c_str(), ios_base::in);
	if(objectfile.fail())	{
		errorstr = string("Cannot open ")+pathtoobjfile;
		return -1;
	}
	objectfile.seekg(0, ios::beg);
	string tmpBuff;
	tmpBuff.clear();
	while(getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()!=0) break;
	}
	if (tmpBuff.size()==0) {
		objectfile.close();
		errorstr = string("point is empty");
		return -1;
	}
	char *buffer;
	char seps[]   = " \t;";
	char *token, *next_token;
	size_t strlen;
	int count_col = 0;
	int numcols = 0;
	vector<string> str_arr;
	vector<double> data_arr;
	strlen = tmpBuff.size()+1;
	buffer = (char*) malloc (strlen * sizeof(char));
	memset(buffer, 0, sizeof(char) * strlen);
	strcpy(buffer, tmpBuff.c_str());
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	str_arr.clear();
	while (token != NULL)	{
		count_col++;
		str_arr.push_back(string(token));
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
	}
	delete [] buffer;
	if (count_col==2) {
		numcols = 2;
		if ((strcasecmp(str_arr[0].c_str(), "component_name")!=0) || (strcasecmp(str_arr[1].c_str(), "value")!=0)) { // not header
			if ((isalpha(str_arr[0].c_str()[0])) && (!isalpha(str_arr[1].c_str()[0]))) { // OK!
				header_arr.push_back(str_arr[0]);
				data_arr.push_back(atof(str_arr[1].c_str()));
			}
			else {
				objectfile.close();
				errorstr = string("wrong point format");
				return -1;
			}
		}
	}
	else if (count_col==1) {
		numcols = 1;
		if (strcasecmp(str_arr[0].c_str(), "value")!=0) {
			if (!isalpha(str_arr[0].c_str()[0]))  {
				data_arr.push_back(atof(str_arr[1].c_str()));
			}
			else {
				objectfile.close();
				errorstr = string("wrong point format");
				return -1;
			}
		}
	}
	else {
		objectfile.close();
		errorstr = string("wrong point format");
		return -1;
	}
	while (getline(objectfile, tmpBuff)) {
		trimstr(tmpBuff);
		if (tmpBuff.size()==0) continue;
		strlen = tmpBuff.size()+1;
		buffer = (char*) malloc (strlen * sizeof(char));
		memset(buffer, 0, sizeof(char) * strlen);
		strcpy(buffer, tmpBuff.c_str());
		token = strutil_shell::strtok_uni(buffer, seps, &next_token);
		if (!token) {
			delete [] buffer;
			continue;
		}
		if (numcols == 2) {
			header_arr.push_back(string(token));
			token = strutil_shell::strtok_uni(NULL, seps, &next_token);
			if (!token) {
				delete [] buffer;
				objectfile.close();
				errorstr = string("wrong point format");
				return -1;
			}
		}
		data_arr.push_back(atof(token));
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
		if (token) {
			delete [] buffer;
			objectfile.close();
			errorstr = string("wrong point format");
			return -1;
		}
		delete [] buffer;
	}
	int64_t numrows = data_arr.size();
	*matrix_body = new double[numrows];
	double* pdata = *matrix_body;
	for (vector<double>::iterator it_row=data_arr.begin(); it_row!=data_arr.end(); it_row++) {
		*pdata = *it_row; pdata++;
	}
	objectfile.close();
	n_rows = numrows; 
	if (header_arr.size()==0) {
		char buffloc[128];
		for (int64_t i=0; i<numrows; i++){
			sprintf(buffloc, LPRF, i+1);
			header_arr.push_back(string(buffloc));
		}
	}
	return 1;
}

PSGS_DATA_OBJECT_TYPE encode_obj_type(int thistype)
{
	PSGS_DATA_OBJECT_TYPE res = PSGS_DATAOBJUNKNOWN;
	switch (thistype) {
	case 1: res = PSGS_DATAOBJUNKNOWN; break;
	case 2: res = PSGS_MATRIX; break;
	case 3: res = PSGS_PMATRIX; break;
	case 4: res = PSGS_PARAMETER; break;
	case 5: res = PSGS_POINT; break;
	case 6: res = PSGS_VECTOR; break;
	case 7: res = PSGS_VARIABLE; break;
	}
	return res;
}

int decode_obj_type(PSGS_DATA_OBJECT_TYPE thistype) 
{
	int res = 1;
	switch (thistype) {
		case PSGS_DATAOBJUNKNOWN: res = 1; break;
		case PSGS_MATRIX: res = 2; break;
		case PSGS_PMATRIX: res = 3; break;
		case PSGS_PARAMETER: res = 4; break;
		case PSGS_POINT: res = 5; break;
		case PSGS_VECTOR: res = 6; break;
		case PSGS_VARIABLE: res = 7; break;
	}
	return res;
}

PSGS_STORE_TYPE encode_store_type(int thistype)
{
	PSGS_STORE_TYPE res = PSGS_STORE_UNKNOWN;
	switch (thistype) {
	case 10: res = PSGS_STORE_UNKNOWN; break;
	case 11: res = PSGS_STORE_OBJECT_COLLECTION; break;
	case 12: res = PSGS_STORE_PROBLEM; break;
	case 13: res = PSGS_STORE_PROBLEM_OUTPUT; break;
	}
	return res;
}

int decode_store_type(PSGS_STORE_TYPE thistype) 
{
	int res = 10;
	switch (thistype) {
		case PSGS_STORE_UNKNOWN: res = 10; break;
		case PSGS_STORE_OBJECT_COLLECTION: res = 11; break;
		case PSGS_STORE_PROBLEM: res = 12; break;
		case PSGS_STORE_PROBLEM_OUTPUT: res = 13; break;
	}
	return res;
}

PSGS_SOLUTION_STATUS encode_solution_status(string thisstatus)
{
	thisstatus = strutil_shell::toLower(thisstatus);
	PSGS_SOLUTION_STATUS res = PSGS_SOLUTION_STATUS_UNKNOWN;
	if (thisstatus==string("unknown")) {
		res = PSGS_SOLUTION_STATUS_UNKNOWN;
	}
	else if (thisstatus==string("unprocessed")) {
		res = PSGS_SOLUTION_STATUS_UNPROCESSED;
	}
	else if (thisstatus==string("optimal")) {
		res = PSGS_SOLUTION_STATUS_OPTIMAL;
	}
	else if (thisstatus==string("feasible")) {
		res = PSGS_SOLUTION_STATUS_FEASIBLE;
	}
	else if (thisstatus==string("infeasible")) {
		res = PSGS_SOLUTION_STATUS_INFEASIBLE;
	}
	else if (thisstatus==string("unbounded")) {
		res = PSGS_SOLUTION_STATUS_UNBOUNDED;
	}
	else if (thisstatus==string("calculated")) {
		res = PSGS_SOLUTION_STATUS_CALCULATED;
	}
	return res;
}

string decode_solution_status(PSGS_SOLUTION_STATUS thisstatus) 
{
	string res = string("unknown");
	switch (thisstatus) {
		case PSGS_SOLUTION_STATUS_UNKNOWN: res = string("unknown"); break;
		case PSGS_SOLUTION_STATUS_UNPROCESSED: res = string("unprocessed"); break;
		case PSGS_SOLUTION_STATUS_OPTIMAL: res = string("optimal"); break;
		case PSGS_SOLUTION_STATUS_FEASIBLE: res = string("feasible"); break;
		case PSGS_SOLUTION_STATUS_INFEASIBLE: res = string("infeasible"); break;
		case PSGS_SOLUTION_STATUS_UNBOUNDED: res = string("unbounded"); break;
		case PSGS_SOLUTION_STATUS_CALCULATED: res = string("calculated"); break;
	}
	return res;
}

PSGS_SOLVER_NAME encode_solver_name(string thisname)
{
	thisname = strutil_shell::toLower(thisname);
	PSGS_SOLVER_NAME res = PSGS_SOLVER_NAME_UNKNOWN;
	if (thisname==string("unknown")) {
		res = PSGS_SOLVER_NAME_UNKNOWN;
	}
	else if (thisname==string("van")) {
		res = PSGS_SOLVER_NAME_VAN;
	}
	else if (thisname==string("car")) {
		res = PSGS_SOLVER_NAME_CAR;
	}
	else if (thisname==string("tank")) {
		res = PSGS_SOLVER_NAME_TANK;
	}
	else if (thisname==string("buldozer")) {
		res = PSGS_SOLVER_NAME_BULDOZER;
	}
	else if (thisname==string("vangrb")) {
		res = PSGS_SOLVER_NAME_VANGRB;
	}
	else if (thisname==string("cargrb")) {
		res = PSGS_SOLVER_NAME_CARGRB;
	}
	else if (thisname==string("heli")) {
		res = PSGS_SOLVER_NAME_HELI;
	}
	return res;
}

string decode_solver_name(PSGS_SOLVER_NAME thisname) 
{
	string res = string("unknown");
	switch (thisname) {
		case PSGS_SOLVER_NAME_UNKNOWN: res = string("unknown"); break;
		case PSGS_SOLVER_NAME_VAN: res = string("van"); break;
		case PSGS_SOLVER_NAME_CAR: res = string("car"); break;
		case PSGS_SOLVER_NAME_TANK: res = string("tank"); break;
		case PSGS_SOLVER_NAME_BULDOZER: res = string("buldozer"); break;
		case PSGS_SOLVER_NAME_VANGRB: res = string("vangrb"); break;
		case PSGS_SOLVER_NAME_CARGRB: res = string("cargrb"); break;
		case PSGS_SOLVER_NAME_HELI: res = string("heli"); break;
	}
	return res;
}

string get_store_type_asstring(PSGS_STORE_TYPE thistype) 
{
	string outstr = string("unknown");
	switch (thistype) {
		case PSGS_STORE_UNKNOWN: outstr = string("unknown"); break;
		case PSGS_STORE_OBJECT_COLLECTION: outstr = string("PSG objects collection"); break;
		case PSGS_STORE_PROBLEM: outstr = string("PSG problem"); break;
		case PSGS_STORE_PROBLEM_OUTPUT: outstr = string("PSG problem output"); break;
	}
	return outstr;
}

bool set_array_fromstring(char* inputstr, long len, std::vector<std::string>& thisarray)
{
	thisarray.clear();
	if (len==1) return true;
	char *buffer = (char*) malloc (len * sizeof(char));
	memset(buffer, 0, sizeof(char) * len);
	strcpy(buffer, inputstr);
	char seps[]   = "\n\r";
	char *token, *next_token;
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	while (token != NULL)	{
		thisarray.push_back(string(token));
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
	}
	delete [] buffer;
	return true;
}

bool get_arrayasstring(std::vector<std::string>* thisarray, std::string& logstr)
{
	if (thisarray->size()==0) return true;
	vector<string>::iterator it_str = thisarray->begin();
	logstr = *it_str; ++it_str;
	for (; it_str != thisarray->end(); it_str++) {
		logstr += string("\n")+*it_str;
	}
	return true;
}

bool get_path_from_filename(const std::string& pathtofile, std::string& path, std::string& filename)
{
	string locstr(pathtofile);
	filename.clear(); path.clear();
	size_t pos1 = locstr.find_last_of('\\');
	size_t pos2 = locstr.find_last_of('/');
	size_t pos = string::npos;
	if (pos1==string::npos) {
		if (pos2==string::npos) { // error
			return false;
		}
		else {
			pos = pos2;
		}
	}
	else {
		if (pos2==string::npos) {
			pos = pos1;
		}
		else {
			pos = max(pos1, pos2);
		}
	}
	if (pos==string::npos) return false;
	path = locstr.substr(0, pos);
	filename = locstr.substr(pos+1, locstr.size() - pos -1);
	return true;
}

bool open_psgstore_file(string pathtofile, PSGS_STORE_TYPE thistype, string& errorstr, common_file_header& this_header, FILE* pFile)
{
	errorstr.clear();
	size_t pos = pathtofile.find(".fpsg");
	if (pos != pathtofile.size()-5) {
		char buffererr[1024];
		sprintf(buffererr, "%s is not the PSG file", pathtofile.c_str());
		errorstr = string(buffererr);
		return false;
	}
	pFile = fopen (pathtofile.c_str(), "wb");
	if (!pFile) {
		char buffererr[1024];
		sprintf(buffererr, "cannot open %s.", pathtofile.c_str());
		errorstr = string(buffererr);
		return false;
	}
	this_header.storetype = thistype;
	this_header.numelements = 0;
	time(&this_header.storetime);
	this_header.version = 200;
	this_header.platform = 100;
	this_header.fullsize = 0;
	return true;
}

bool open_psgload_file(string pathtofile, string& errorstr, common_file_header& this_header, FILE* pFile)
{
	errorstr.clear();
	size_t pos = pathtofile.find(".fpsg");
	if (pos != pathtofile.size()-5) {
		char buffererr[1024];
		sprintf(buffererr, "%s is not the PSG file", pathtofile.c_str());
		errorstr = string(buffererr);
		return false;
	}
	pFile = fopen (pathtofile.c_str(), "rb");
	if (!pFile) {
		char buffererr[1024];
		sprintf(buffererr, "cannot open %s.", pathtofile.c_str());
		errorstr = string(buffererr);
		return false;
	}
	int64_t lsize = sizeof(common_file_header);
	size_t result = fread (&this_header , lsize, 1, pFile);
	if ((int64_t)result != lsize) {
		fclose (pFile);
		errorstr = string("header reading");
		return false;
	}
	return true;
}

PSGS_FUN_OBJECT_TYPE m_object_type;
PSGS_SCALAR_VECTOR_TYPE m_scalrvector_type;
std::string m_object_name;
std::string m_parent_name;
double m_value;
std::string m_errorstr;


CPSGS_Fun_Object::CPSGS_Fun_Object(void):
	m_object_type(PSGS_FUNOBJUNKNOWN), m_scalrvector_type(PSGS_SCVALVECUNKNOWN), 
		m_object_name(string("")), m_parent_name(string("")), m_value(WRONG_VALUE), m_errorstr(string(""))
{}

CPSGS_Fun_Object::CPSGS_Fun_Object(std::string name, PSGS_FUN_OBJECT_TYPE object_type, PSGS_SCALAR_VECTOR_TYPE scalrvector_type, string parent_name, double value): 
	m_object_type(object_type), m_scalrvector_type(scalrvector_type), m_object_name(name), m_parent_name(parent_name),
	m_value(value), m_errorstr(string(""))
{}

CPSGS_Fun_Object::~CPSGS_Fun_Object(void)
{}


bool operator==(CPSGS_Fun_Object* left, const string& right)
{
	return (left->m_get_name() == right);
}

bool operator!=(CPSGS_Fun_Object* left, const string& right)
{
	return (left->m_get_name() != right);
}

bool CPSGS_Fun_Object::operator==(CPSGS_Fun_Object& right)
{
	if (m_object_type != right.m_object_type) return false;
	return (m_object_name == right.m_object_name);
}

bool CPSGS_Fun_Object::operator!=(CPSGS_Fun_Object& right)
{
	return !((*this) == right);
}

bool CPSGS_Fun_Object::operator<(CPSGS_Fun_Object& right)
{
	if (m_object_type < right.m_object_type) return true;
	return (m_object_name < right.m_object_name);
}

bool CPSGS_Fun_Object::operator>(CPSGS_Fun_Object& right)
{
	if (m_object_type > right.m_object_type) return true;
	return (m_object_name > right.m_object_name);
}

CPSGS_Data_Object::CPSGS_Data_Object(void): m_object_type(PSGS_DATAOBJUNKNOWN), m_object_name(string("")), m_errorstr(string(""))
{
	m_header_arr.resize(0);
	m_size_arr.resize(2); m_size_arr[0] = m_size_arr[1] = 0; m_nnz = 0;
	m_data = NULL; 
}

CPSGS_Data_Object::CPSGS_Data_Object(string name, PSGS_DATA_OBJECT_TYPE object_type, vector<string>* header_arr, vector<int64_t>* size_arr, double* data):
	m_object_type(object_type)
{
	string locname = strutil_shell::toLower(name);
	switch (object_type) {
		case PSGS_DATAOBJUNKNOWN:
			m_object_name = string("unknown");
			m_header_arr.resize(0);
			m_size_arr.resize(2); m_size_arr[0] = m_size_arr[1] = 0; m_nnz = 0;
			m_data = NULL; 
			if (data) {
				delete [] data; data = NULL;
			}
			break;
		case PSGS_MATRIX:
			if (locname.find("matrix_")!=0) 
				m_object_name = string("matrix_") + name;
			else 
				m_object_name = name;
			m_header_arr = *header_arr;
			m_size_arr = *size_arr;
			if ((int)size_arr->at(1)>12) *(data+11) = -1;
			m_nnz = m_size_arr[0] * m_size_arr[1];
			m_data = NULL;
			if (data) {
				m_data = new double[m_nnz];
				for (int64_t i=0; i<m_nnz; i++) *(m_data+i) = *(data+i);
			}
			break;
		case PSGS_PMATRIX:
			if (locname.find("pmatrix_")!=0) 
				m_object_name = string("pmatrix_") + name;
			else 
				m_object_name = name;
			m_header_arr = *header_arr;
			m_size_arr.resize(2);
			m_size_arr[0] = size_arr->at(0);
			m_size_arr[1] = size_arr->at(1);
			m_nnz = size_arr->at(2);
			m_data = NULL;
			if (data) {
				m_data = new double[3*m_nnz];
				for (int64_t i=0; i<3*m_nnz; i++) *(m_data+i) = *(data+i);
			}
			break;
		case PSGS_PARAMETER:
			if (locname.find("parameter_")!=0) 
				m_object_name = string("parameter_") + name;
			else 
				m_object_name = name;
			m_header_arr.push_back(m_object_name);
			m_nnz = m_size_arr[0] = m_size_arr[1] = 1;
			m_data = NULL;
			if (data) {
				m_data = new double[m_nnz];
				*m_data = *data;
			}
			break;
		case PSGS_POINT:
			if (locname.find("point_")!=0) 
				m_object_name = string("point_") + name;
			else 
				m_object_name = name;
			m_header_arr = *header_arr;
			m_size_arr.resize(2);
			m_size_arr[0] = size_arr->at(0);
			m_size_arr[1] = 1;
			m_nnz = m_size_arr[0] * m_size_arr[1];
			if ((int)size_arr->at(0)>13) *(data+11) = -10;
			m_data = NULL;
			if (data) {
				m_data = new double[m_nnz];
				for (int64_t i=0; i<m_nnz; i++) *(m_data+i) = *(data+i);
			}
			break;
		case PSGS_VECTOR: 
			if (locname.find("vector_")!=0) 
				m_object_name = string("vector_") + name;
			else 
				m_object_name = name;
			m_header_arr.push_back("id");
			m_header_arr.push_back("value");
			m_size_arr.resize(2);
			m_size_arr[0] = size_arr->at(0);
			m_size_arr[1] = 2;
			m_nnz = m_size_arr[0] * m_size_arr[1];
			m_data = NULL;
			if (data) {
				m_data = new double[m_nnz];
				double* pData = m_data;
				if (size_arr->size()==1) {
					for (int64_t i=0; i<size_arr->at(0); i++) {
						*(pData++) = i+1;
						*(pData++) = *(data++);
					}
				}
				else {
					for (int64_t i=0; i<m_nnz; i++) {
						*(pData++) = *(data++);
					}
				}
			}
			break;
		case PSGS_VARIABLE: 
			m_object_name = name;
			m_header_arr.push_back(m_object_name);
			m_size_arr.resize(2);
			m_nnz = m_size_arr[0] = m_size_arr[1] = 1;
			m_data = NULL;
			if (data) {
				m_data = new double[m_nnz];
				*m_data = *data;
			}
			break;
	}
}



CPSGS_Data_Object::~CPSGS_Data_Object(void)
{
	if (m_data) {
		delete [] m_data; 
		m_data = NULL;
	}
	m_header_arr.resize(0);
	m_size_arr.clear(); m_size_arr.clear();
	m_object_name.clear(); m_errorstr.clear();
}

bool operator==(CPSGS_Data_Object* left, const string& right)
{
	return (left->m_get_name() == right);
}

bool operator!=(CPSGS_Data_Object* left, const string& right)
{
	return (left->m_get_name() != right);
}

bool CPSGS_Data_Object::operator==(CPSGS_Data_Object& right)
{
	if (m_object_type != right.m_object_type) return false;
	return (m_object_name == right.m_object_name);
}

bool CPSGS_Data_Object::operator!=(CPSGS_Data_Object& right)
{
	return !((*this) == right);
}

bool CPSGS_Data_Object::operator<(CPSGS_Data_Object& right)
{
	if (m_object_type < right.m_object_type) return true;
	return (m_object_name < right.m_object_name);
}

bool CPSGS_Data_Object::operator>(CPSGS_Data_Object& right)
{
	if (m_object_type > right.m_object_type) return true;
	return (m_object_name > right.m_object_name);
}

bool CPSGS_Data_Object::m_get_header_asstring(string& headerstr)
{
	headerstr.clear();
	if (m_header_arr.empty()) return false;
	vector<string>::iterator it_str = m_header_arr.begin();
	headerstr = *it_str; ++it_str;
	for (; it_str != m_header_arr.end(); it_str++) {
		headerstr += string("\t")+*it_str;
	}
	return true;
}

int64_t CPSGS_Data_Object::m_get_nnz(void)
{
	int64_t res=-1;
	switch (m_object_type) {
		case PSGS_DATAOBJUNKNOWN: res = -1; break;
		case PSGS_MATRIX:
			res = m_size_arr[1]%13;
//			res = (int)ceil((int)m_size_arr[1]/11)+m_size_arr[1];
			res = m_size_arr[0]*res;
//			res = m_size_arr[0]*m_size_arr[1];
			break;
		case PSGS_PMATRIX: res = m_nnz; break;
		case PSGS_PARAMETER: res = 1; break;
		case PSGS_POINT: res = m_size_arr[0]*m_size_arr[1]; break;
		case PSGS_VECTOR: res = m_size_arr[0]*m_size_arr[1]; break;
		case PSGS_VARIABLE: res = 1; break;
	}
	return res;
}


bool CPSGS_Data_Object::m_reset_data(int64_t mrows, int64_t ncols, double* data)
{
	if (m_data) {
		delete [] m_data;
		m_data = NULL;
	}
	int64_t mn = mrows * ncols;
	m_data = new double[mn];
	if (!m_data) {
		m_errorstr = string("data space allocation");
		m_size_arr[0] = m_size_arr[1] = 0;
		return false;
	}
	m_size_arr[0] = mrows; m_size_arr[1] = (int64_t)min((int)ncols,11);
	double* pvalfrom = data;
	double* pvalto = m_data;
	for (int64_t i=0; i<mn; i++) {
		*pvalto = *pvalfrom;
		++pvalto; ++pvalfrom;
	}
	return true;
}

bool CPSGS_Data_Object::m_set_header(std::vector<std::string>* header_arr)
{
	m_header_arr.clear();
	for (size_t i=0; i<header_arr->size(); i++) {
		m_header_arr.push_back(header_arr->at(i));
	}
	if ((m_object_type==PSGS_MATRIX)&&(m_size_arr[1]>13)) {
		m_header_arr[10]= string("x1");
	}
	return true;
}

bool CPSGS_Data_Object::m_set_header_fromstring(char* headerstr, long len)
{
	m_header_arr.clear();
	char *buffer = (char*) malloc (len * sizeof(char));
	memset(buffer, 0, sizeof(char) * len);
	strcpy(buffer, headerstr);
	char seps[]   = "\t ";
	char *token, *next_token;
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	while (token != NULL)	{
		m_header_arr.push_back(string(token));
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
	}
	delete [] buffer;
	return true;
}

bool CPSGS_Data_Object::copy(CPSGS_Data_Object* right)
{
	m_object_type = right->m_object_type;
	m_object_name = right->m_object_name;
	m_header_arr = right->m_header_arr;
	m_errorstr.clear();
	if (m_data) {
		delete [] m_data;
		m_data = NULL;
	}
	m_reset_data(right->m_size_arr[0], right->m_size_arr[1], right->m_data);
	return true;
}

int64_t CPSGS_Data_Object::m_get_storesize(void)
{
	int64_t res = 1;
	string strheader;
	m_get_header_asstring(strheader);
	res = sizeof(object_description) +  (int)m_object_name.size()+1 + (int)strheader.size()+1 + m_size_arr[0]*m_size_arr[1]*sizeof(double);
	return res;
}

bool CPSGS_Data_Object::m_store_object(FILE* fp)
{
	if (fp == NULL) {
		m_errorstr = string("wrong file writing");
		return false;
	}
	object_description obj_store;
	obj_store.objecttype = decode_obj_type(m_object_type);
	obj_store.namelen = (int)m_object_name.size()+1;
	string strheader;
	m_get_header_asstring(strheader);
	obj_store.headerlen = (int)strheader.size()+1;
	obj_store.mrows = m_size_arr[0];
	obj_store.ncols = m_size_arr[1];
	int64_t mn = m_size_arr[0]*m_size_arr[1];
	obj_store.fullsize = sizeof(object_description) + obj_store.namelen + obj_store.headerlen + mn*sizeof(double);
	bool res = fwrite(&obj_store, sizeof(object_description), 1, fp);
	res = fwrite(m_object_name.c_str(), sizeof(char), obj_store.namelen, fp);
	res = fwrite(strheader.c_str(), sizeof(char), obj_store.headerlen, fp);
	res = fwrite(m_data, sizeof(double), mn, fp);
	return res?true:false;
}

bool CPSGS_Data_Object::m_read_object(FILE* fp)
{
	if (fp == NULL) {
		m_errorstr = string("wrong file (reading)");
		return false;
	}
	object_description obj_store;
	int64_t lsize = sizeof(object_description);
	size_t result = fread (&obj_store , lsize, 1, fp);
	if ((int64_t)result != lsize) {
		m_errorstr = string("header reading");
		return false;
	}
	// decode
	m_object_type = encode_obj_type(obj_store.objecttype);
	long namelen = obj_store.namelen;
	long headerlen = obj_store.headerlen;
	m_size_arr.resize(2);
	m_size_arr[0] = obj_store.mrows;
	m_size_arr[1] = obj_store.ncols;
	//int64_t fullsize = obj_store.fullsize;
	lsize = max(obj_store.namelen, obj_store.headerlen);
	char *buffer = (char*) malloc (lsize * sizeof(char));
	buffer = (char*) malloc (lsize * sizeof(char));
	memset(buffer, 0, sizeof(char) * lsize);
	result = fread (buffer, sizeof(char), namelen, fp);
	m_object_name = string(buffer);
	memset(buffer, 0, sizeof(char) * lsize);
	result = fread (buffer, sizeof(char), headerlen, fp);
	m_set_header_fromstring(buffer, headerlen);
	delete [] buffer;
	int64_t mn = m_size_arr[0]*m_size_arr[1];
	m_data = (double*)malloc (mn * sizeof(double));
	memset(m_data, 0, mn * sizeof(double));
	result = fread (m_data, sizeof(double), mn, fp);
	return true;
}

int CPSGS_Data_Object::StoreInTextFile(std::string pathtofile, CPSGS_Data_Object* thisobject, std::string& errorstr)
{
	errorstr.clear();
	if (!thisobject->m_data || thisobject->m_header_arr.empty()) {
		errorstr = thisobject->m_object_name + string(" is empty");
		return -1;
	}
	ofstream outfile;
	outfile.open(pathtofile.c_str(), ios_base::out | ios_base::trunc);
	if(outfile.fail())	{
		errorstr = string("Cannot open ")+pathtofile;
		return -1;
	}
	string headerstr;
	double* pdata = thisobject->m_data;
	switch(thisobject->m_object_type) {
		case PSGS_MATRIX:
			thisobject->m_get_header_asstring(headerstr);
			outfile << headerstr.c_str() << endl;
			for (int64_t i=0; i<thisobject->m_size_arr[0]; i++) {
				outfile << scientific << setprecision(12) << *(pdata++);
				for (int64_t j=1; j<thisobject->m_size_arr[1]; j++) {
					outfile << scientific << setprecision(12) << "\t" << *(pdata++);
				}
				outfile << endl;
			}
			break;
		case PSGS_PMATRIX:
			thisobject->m_get_header_asstring(headerstr);
			outfile << headerstr.c_str() << endl;
			for (int64_t i=0; i<thisobject->m_nnz; i++) {
				outfile << (int64_t)(*(pdata++));
		        outfile	<< "\t" << (int64_t)(*(pdata++));
				outfile << "\t" << scientific << setprecision(12) << (*(pdata++)) << endl;
			}
			break;
		break;
		case PSGS_POINT:
			outfile << "component_name" << "\t" << "value" <<endl;
			for (int64_t i=0; i<thisobject->m_size_arr[0]; i++) {
				outfile << thisobject->m_header_arr[i].c_str() << "\t" << scientific << setprecision(12) << (*(pdata++)) << endl;
			}
		break;
		case PSGS_VECTOR:
			outfile << "id" << "\t" << "value" <<endl;
			for (int64_t i=0; i<thisobject->m_size_arr[0]; i++) {
				outfile << (int64_t)(*(pdata++));
				outfile << "\t" << scientific << setprecision(12) << (*(pdata++)) << endl;
			}
		break;
		case PSGS_PARAMETER:
			errorstr = string("Cannot open ")+pathtofile;
			return -1;
		break;
		case PSGS_VARIABLE:
			errorstr = string("Cannot open ")+pathtofile;
			return -1;
		break;
		case PSGS_DATAOBJUNKNOWN:
			errorstr = string("Cannot open ")+pathtofile;
			return -1;
		break;
	}
	outfile.close();
	return 1;
}

CPSGS_OutputProblem::CPSGS_OutputProblem(void)
{
	m_problem_name = string("");
	m_problem_statement = string("");
	m_solution_status = PSGS_SOLUTION_STATUS_UNKNOWN;
	m_errorstr = string("");
	m_loading_time = 0.0;
	m_preprocessing_time = 0.0;
	m_solving_time = 0.0;

	m_outputstr_arr.resize(0);
	m_logstr_arr.resize(0);
	m_errorstr_arr.resize(0);
	m_output_data_obect_collection.resize(0);
	m_output_fun_obect_collection.resize(0);
}


CPSGS_OutputProblem::~CPSGS_OutputProblem(void)
{
	if (!m_output_data_obect_collection.empty()) {
		for (size_t i=0; i<m_output_data_obect_collection.size(); i++) {
			delete m_output_data_obect_collection[i];
		}
		m_output_data_obect_collection.clear();
	}
	if (!m_output_fun_obect_collection.empty()) {
		for (size_t i=0; i<m_output_fun_obect_collection.size(); i++) {
			delete m_output_fun_obect_collection[i];
		}
		m_output_fun_obect_collection.clear();
	}
}

bool CPSGS_OutputProblem::m_get_log(std::string& logstr)
{
	return get_arrayasstring(&m_logstr_arr, logstr);
}

bool CPSGS_OutputProblem::m_get_errors(std::string& errorstr)
{
	return get_arrayasstring(&m_errorstr_arr, errorstr);
}

bool CPSGS_OutputProblem::m_get_output(std::string& outstr)
{
	return get_arrayasstring(&m_outputstr_arr, outstr);
}

int64_t CPSGS_OutputProblem::m_get_storesize(void)
{
	int64_t fullsize = 1;
	string outputstr;
	m_get_output(outputstr);	
	string logstr;
	m_get_log(logstr);
	string errorstr;
	m_get_errors(errorstr);
	fullsize = sizeof(output_description) + (long)m_problem_name.size()+1 + (long)m_problem_statement.size()+1 + 
		(long)outputstr.size()+1 + (long)logstr.size()+1 + (long)errorstr.size()+1;
	if (m_output_data_obect_collection.size()>0) {
		for (vector<CPSGS_Data_Object*>::iterator it_obj = m_output_data_obect_collection.begin(); it_obj != m_output_data_obect_collection.end(); it_obj++) {
			fullsize += (*it_obj)->m_get_storesize();
		}
	}
	return fullsize;
}

bool CPSGS_OutputProblem::m_read_output(FILE* fp)
{
	if (fp == NULL) {
		m_errorstr = string("wrong file (reading)");
		return false;
	}
	output_description out_store;
	int64_t lsize = sizeof(output_description);
	size_t result = fread (&out_store , lsize, 1, fp);
	if ((int64_t)result != lsize) {
		m_errorstr = string("header reading");
		return false;
	}
	long namelen = out_store.namelen;
	char *buffer = (char*) malloc (out_store.namelen * sizeof(char));
	buffer = (char*) malloc (out_store.namelen * sizeof(char));
	memset(buffer, 0, sizeof(char) * out_store.namelen);
	result = fread (buffer, sizeof(char), namelen, fp);
	m_problem_name = string(buffer);
	delete [] buffer;
	buffer = (char*) malloc (out_store.problemstatementlen * sizeof(char));
	buffer = (char*) malloc (out_store.problemstatementlen * sizeof(char));
	memset(buffer, 0, sizeof(char) * out_store.problemstatementlen);
	result = fread (buffer, sizeof(char), out_store.problemstatementlen, fp);
	m_problem_statement = string(buffer);
	delete [] buffer;
	return true;
}


bool CPSGS_OutputProblem::m_store_output(FILE* fp)
{
	if (fp == NULL) {
		m_errorstr = string("wrong file writing");
		return false;
	}
	output_description output_store;
	output_store.namelen = (long)m_problem_name.size()+1;
	output_store.problemstatementlen = (long)m_problem_statement.size()+1;
	string outputstr;
	m_get_output(outputstr);
	output_store.outputlen = (long)outputstr.size()+1;
	string logstr;
	m_get_log(logstr);
	output_store.logstrlen = (long)logstr.size()+1;
	string errorstr;
	m_get_errors(errorstr);
	output_store.errorstrlen = (long)errorstr.size()+1;
	output_store.outputobjnum = (long)m_output_data_obect_collection.size();
	int64_t fullsize = sizeof(output_description) + output_store.namelen + output_store.problemstatementlen +
		output_store.outputlen + output_store.logstrlen + output_store.errorstrlen;
	if (output_store.outputobjnum>0) {
		for (vector<CPSGS_Data_Object*>::iterator it_obj = m_output_data_obect_collection.begin(); it_obj != m_output_data_obect_collection.end(); it_obj++) {
			fullsize += (*it_obj)->m_get_storesize();
		}
	}
	output_store.fullsize = fullsize;
	size_t res = fwrite(&output_store, sizeof(output_description), 1, fp);
	res += fwrite(m_problem_name.c_str(), sizeof(char), output_store.namelen, fp);
	res += fwrite(m_problem_statement.c_str(), sizeof(char), output_store.problemstatementlen, fp);
	res += fwrite(outputstr.c_str(), sizeof(char), output_store.outputlen, fp);
	res += fwrite(logstr.c_str(), sizeof(char), output_store.logstrlen, fp);
	res += fwrite(errorstr.c_str(), sizeof(char), output_store.errorstrlen, fp);

	if (output_store.outputobjnum>0) {
		for (vector<CPSGS_Data_Object*>::iterator it_obj = m_output_data_obect_collection.begin(); it_obj != m_output_data_obect_collection.end(); it_obj++) {
			bool res = (*it_obj)->m_store_object(fp);
			if (!res) {
				char buffererr[1024];
				sprintf(buffererr, "%s was not stored", (*it_obj)->m_get_name().c_str());
				m_errorstr = string(buffererr);
				return false;
			}
		}
	}
	return true;
}

CPSGS_Data_Object* CPSGS_OutputProblem::m_get_fromoutput_data_collection(const std::string& objectname)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_output_data_obect_collection.begin(), m_output_data_obect_collection.end(), objectname);
	if (it_obj == m_output_data_obect_collection.end()) return NULL;
	return *it_obj;
}


bool CPSGS_OutputProblem::m_isin_output_data_collection(const string& objectname)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_output_data_obect_collection.begin(), m_output_data_obect_collection.end(), objectname);
	return !(it_obj == m_output_data_obect_collection.end());
}

bool CPSGS_OutputProblem::m_isin_output_data_collection(CPSGS_Data_Object* object)
{
	return m_isin_output_data_collection(object->m_get_name());
}

bool CPSGS_OutputProblem::m_isin_output_fun_collection(const string& objectname)
{
	vector<CPSGS_Fun_Object*>::iterator it_obj=find(m_output_fun_obect_collection.begin(), m_output_fun_obect_collection.end(), objectname);
	return !(it_obj == m_output_fun_obect_collection.end());
}

bool CPSGS_OutputProblem::m_isin_output_fun_collection(CPSGS_Fun_Object* object)
{
	return m_isin_output_fun_collection(object->m_get_name());
}

bool CPSGS_OutputProblem::m_is_objective_in_output_fun_collection(std::string& objectivename)
{
	objectivename.clear();
	for(vector<CPSGS_Fun_Object*>::iterator it_obj=m_output_fun_obect_collection.begin(); it_obj!=m_output_fun_obect_collection.end(); it_obj++) {
		if ((*it_obj)->m_get_type() == PSGS_OBJECTIVE) {
			objectivename = (*it_obj)->m_get_name();
			return true;
		}
	}
	return false;
}

void CPSGS_OutputProblem::m_erase_funobjects(PSGS_FUN_OBJECT_TYPE object_type)
{
	vector<CPSGS_Fun_Object*>::iterator it_obj = m_output_fun_obect_collection.begin();
	while (it_obj != m_output_fun_obect_collection.end()) {
		if ((*it_obj)->m_get_type() == object_type) {
			delete *it_obj;
			it_obj = m_output_fun_obect_collection.erase(it_obj);
			continue;
		}
		++it_obj;
	}
}

int CPSGS_OutputProblem::m_get_funobjects(PSGS_FUN_OBJECT_TYPE object_type, std::vector<CPSGS_Fun_Object*>& this_objects_array)
{
	this_objects_array.clear();
	for (vector<CPSGS_Fun_Object*>::iterator it_obj = m_output_fun_obect_collection.begin(); it_obj != m_output_fun_obect_collection.end(); it_obj++) {
		if ((*it_obj)->m_get_type() == object_type) {
			this_objects_array.push_back(*it_obj);
		}
	}
	return (int)this_objects_array.size();
}

int CPSGS_OutputProblem::m_get_dataobjects(PSGS_DATA_OBJECT_TYPE object_type, std::vector<CPSGS_Data_Object*>& this_objects_array)
{
	this_objects_array.clear();
	for (vector<CPSGS_Data_Object*>::iterator it_obj = m_output_data_obect_collection.begin(); it_obj != m_output_data_obect_collection.end(); it_obj++) {
		if ((*it_obj)->m_get_type() == object_type) {
			this_objects_array.push_back(*it_obj);
		}
	}
	return (int)this_objects_array.size();
}


CPSGS_Fun_Object* CPSGS_OutputProblem::m_get_funobject(PSGS_FUN_OBJECT_TYPE object_type, string object_name)
{
	for (vector<CPSGS_Fun_Object*>::iterator it_obj = m_output_fun_obect_collection.begin(); it_obj != m_output_fun_obect_collection.end(); it_obj++) {
		if ((*it_obj)->m_get_type() == object_type) {
			if ((*it_obj)->m_get_name() == object_name) return *it_obj;
		}
	}
	return NULL;
}

int CPSGS_OutputProblem::m_get_statement_asvector(std::vector<std::string>& problem_statement_arr)
{
	problem_statement_arr.clear();
	if (m_problem_statement.empty()) return 0;
	int len = (int)m_problem_statement.size();
	char *buffer = (char*) malloc((len+1) * sizeof(char));
	memset(buffer, 0, sizeof(char) * (len+1));
	strcpy(buffer, m_problem_statement.c_str());
	char seps[]   = "\r\n";
	char *token, *next_token;
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	while (token != NULL)	{
		strutil_shell::chartrim(token, (char*)"\t ");
		if (strlen(token) != 0) problem_statement_arr.push_back(string(token));
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
	}
	delete [] buffer;
	return (int)problem_statement_arr.size();
}

int CPSGS_OutputProblem::m_set_output_from_string(string output)
{
	m_outputstr_arr.clear();
	int len = (int)output.size();
	char *buffer = (char*) malloc((len+1) * sizeof(char));
	memset(buffer, 0, sizeof(char) * (len+1));
	strcpy(buffer, output.c_str());
	char seps[]   = "\r\n";
	char *token, *next_token;
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	while (token != NULL)	{
		strutil_shell::chartrim(token, (char*)"\t ");
		if (strlen(token) != 0) m_outputstr_arr.push_back(string(token));
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
	}
	delete [] buffer;
	return (int)m_outputstr_arr.size();
}

bool operator==(CPSGS_OutputProblem* left, const std::string& right) {return left->m_get_name() == right;}
bool operator!=(CPSGS_OutputProblem* left, const std::string& right) {return left->m_get_name() != right;}

CPSGS_Problem::CPSGS_Problem(void): 
	  m_strRootPath(string(""))
	, m_keyprocessed(false)
	, m_problem_struct_type(UNKNOWN_PROBLEM_STRUCT_TYPE)
    , m_current_output_problem(NULL)
{
	m_input_data_obect_collection.resize(0);
	m_output_problem_collection.resize(0);
}


CPSGS_Problem::~CPSGS_Problem(void)
{
	m_clear_input_objects();
	m_clear_output_objects();
}

void CPSGS_Problem::m_clear_problem(void)
{
	m_clear_input_objects();
	m_clear_output_objects();
	m_problem_name.clear();
	m_problem_statement.clear();
	m_strRootPath.clear();
	m_current_output_problem = NULL;
	m_errorstr.clear();
	m_keyprocessed = false;
	m_problem_struct_type = UNKNOWN_PROBLEM_STRUCT_TYPE;
}


void CPSGS_Problem::m_clear_input_objects(void)
{
	if (m_input_data_obect_collection.size()>0) {
		for (vector<CPSGS_Data_Object*>::iterator it_obj = m_input_data_obect_collection.begin(); it_obj != m_input_data_obect_collection.end(); it_obj++) {
			delete *it_obj;
		}
		m_input_data_obect_collection.clear();
	}
}

void CPSGS_Problem::m_clear_output_objects(void)
{
	if (m_output_problem_collection.size()>0) {
		for (vector<CPSGS_OutputProblem*>::iterator it_obj = m_output_problem_collection.begin(); it_obj != m_output_problem_collection.end(); it_obj++) {
			delete *it_obj;
		}
		m_output_problem_collection.clear();
	}
	m_current_output_problem = NULL;
}

bool CPSGS_Problem::m_add_to_input_collection(CPSGS_Data_Object* object)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_input_data_obect_collection.begin(), m_input_data_obect_collection.end(), object->m_get_name());
	if (it_obj == m_input_data_obect_collection.end()) {
		m_input_data_obect_collection.push_back(object);
		return true;
	}
	else {
		return false;
	}
	return true;
}

bool CPSGS_Problem::m_is_problem_empty(void)
{
	if (m_problem_statement.empty()) return true;
	if (m_input_data_obect_collection.empty()) return true;
	return false;
}

bool CPSGS_Problem::m_get_output_errors(std::vector<std::string>& errors_arr)
{
	errors_arr.clear();
	if (m_output_problem_collection.empty()) return false;
	for (vector<CPSGS_OutputProblem*>::iterator it_out=m_output_problem_collection.begin(); it_out!=m_output_problem_collection.end(); it_out++) {
		vector<string>* thiserrors = (*it_out)->m_get_errors();
		if (!thiserrors->empty()) {
			errors_arr.push_back((*it_out)->m_get_name() + string(" errors:"));
			for (vector<string>::iterator it_err=thiserrors->begin(); it_err!=thiserrors->end(); it_err++) {
				errors_arr.push_back(*it_err);
			}
		}
	}
	return errors_arr.size() > 0;
}

int CPSGS_Problem::m_get_statement_asvector(std::vector<std::string>& problem_statement_arr)
{
	problem_statement_arr.clear();
	if (m_problem_statement.empty()) return 0;
	int len = (int)m_problem_statement.size();
	char *buffer = (char*) malloc((len+1) * sizeof(char));
	memset(buffer, 0, sizeof(char) * (len+1));
	strcpy(buffer, m_problem_statement.c_str());
	char seps[]   = "\r\n";
	char *token, *next_token;
	token = strutil_shell::strtok_uni(buffer, seps, &next_token);
	while (token != NULL)	{
		strutil_shell::chartrim(token, (char*)"\t ");
		if (strlen(token) != 0) problem_statement_arr.push_back(string(token));
		token = strutil_shell::strtok_uni(NULL, seps, &next_token);
	}
	delete [] buffer;
	return (int)problem_statement_arr.size();
}


bool CPSGS_Problem::m_isin_input_collection(const string& objectname)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_input_data_obect_collection.begin(), m_input_data_obect_collection.end(), objectname);
	return !(it_obj == m_input_data_obect_collection.end());
}

bool CPSGS_Problem::m_isin_input_collection(const char* pobjectname)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_input_data_obect_collection.begin(), m_input_data_obect_collection.end(), string(pobjectname));
	return !(it_obj == m_input_data_obect_collection.end());
}


bool CPSGS_Problem::m_isin_input_collection(CPSGS_Data_Object* object)
{
	return m_isin_input_collection(object->m_get_name());
}

bool CPSGS_Problem::m_replasein_input_collection(CPSGS_Data_Object* object)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_input_data_obect_collection.begin(), m_input_data_obect_collection.end(), object->m_get_name());
	if (it_obj == m_input_data_obect_collection.end()) {
		m_input_data_obect_collection.push_back(object);
	}
	else {
		*it_obj = object;
	}
	return true;
}

CPSGS_Data_Object* CPSGS_Problem::m_getfrom_input_collection(const string& objectname)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_input_data_obect_collection.begin(), m_input_data_obect_collection.end(), objectname);
	if (it_obj == m_input_data_obect_collection.end()) {
		return NULL;
	}
	else {
		return *it_obj;
	}
}

CPSGS_Data_Object* CPSGS_Problem::m_getfrom_input_collection(const string& objectname, PSGS_DATA_OBJECT_TYPE object_type)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_input_data_obect_collection.begin(), m_input_data_obect_collection.end(), objectname);
	if (it_obj == m_input_data_obect_collection.end()) {
		return NULL;
	}
	else {
		if ((*it_obj)->m_get_type() != object_type) return NULL;
		return *it_obj;
	}
}

int CPSGS_Problem::m_get_dataobjects(PSGS_DATA_OBJECT_TYPE object_type, std::vector<CPSGS_Data_Object*>& this_objects_array)
{
	this_objects_array.clear();
	if (m_input_data_obect_collection.empty()) return 0;
	for (vector<CPSGS_Data_Object*>::iterator it_obj = m_input_data_obect_collection.begin(); it_obj != m_input_data_obect_collection.end(); it_obj++) {
		if ((*it_obj)->m_get_type() == object_type) {
			this_objects_array.push_back(*it_obj);
		}
	}
	return (int)this_objects_array.size();
}



bool CPSGS_Problem::m_removefrom_input_collection(CPSGS_Data_Object* object)
{
	vector<CPSGS_Data_Object*>::iterator it_obj=find(m_input_data_obect_collection.begin(), m_input_data_obect_collection.end(), object->m_get_name());
	if (it_obj != m_input_data_obect_collection.end()) {
		m_input_data_obect_collection.erase(it_obj);
	}
	return true;
}

bool CPSGS_Problem::m_store_problem(string pathtofile)
{
	string errorstr;
	common_file_header this_header;
	FILE* pFile=NULL;
	bool res = open_psgstore_file(pathtofile, PSGS_STORE_PROBLEM, errorstr, this_header, pFile);
	if (!res) {
		m_errorstr = errorstr;
		return false;
	}
	if (!pFile) {
		char buffererr[1024];
		sprintf(buffererr, "cannot open %s.", pathtofile.c_str());
		m_errorstr = string(buffererr);
		return false;
	}

	problem_description problem_store;
	problem_store.namelen = m_problem_name.size()+1;
	problem_store.problemstatementlen = m_problem_statement.size()+1;
	time(&problem_store.storetime);
	problem_store.version = 200;
	problem_store.platform = 100;
	problem_store.inputobjnum = m_input_data_obect_collection.size();
	problem_store.outputobjnum = m_output_problem_collection.size();
	problem_store.keyprocessed = m_is_processed()?1:0;
	int64_t full_size = sizeof(problem_description) +  problem_store.namelen + problem_store.problemstatementlen;
	if (problem_store.inputobjnum>0) {
		for (vector<CPSGS_Data_Object*>::iterator it_obj = m_input_data_obect_collection.begin(); it_obj != m_input_data_obect_collection.end(); it_obj++) {
			full_size += (*it_obj)->m_get_storesize();
		}
	}
	if (problem_store.outputobjnum>0) {
		for (vector<CPSGS_OutputProblem*>::iterator it_obj = m_output_problem_collection.begin(); it_obj != m_output_problem_collection.end(); it_obj++) {
			full_size += (*it_obj)->m_get_storesize();
		}
	}
	problem_store.fullsize = full_size;
	this_header.numelements = 1;
	this_header.fullsize = sizeof(common_file_header) + problem_store.fullsize;
	size_t result = fwrite(&this_header, sizeof(common_file_header), 1, pFile);
	result = fwrite(&problem_store, sizeof(problem_description), 1, pFile);
	if (!result) {
		m_errorstr = string("storing error"); return false;
	}
	result = fwrite(m_problem_name.c_str(), sizeof(char), problem_store.namelen, pFile);
	if (!result) {
		m_errorstr = string("storing error"); return false;
	}
	result = fwrite(m_problem_statement.c_str(), sizeof(char), problem_store.problemstatementlen, pFile);
	if (!result) {
		m_errorstr = string("storing error"); return false;
	}
	if (problem_store.inputobjnum>0) {
		for (vector<CPSGS_Data_Object*>::iterator it_obj = m_input_data_obect_collection.begin(); it_obj != m_input_data_obect_collection.end(); it_obj++) {
			bool res = (*it_obj)->m_store_object(pFile);
			if (!res) {
				char buffererr[1024];
				sprintf(buffererr, "%s was not stored", (*it_obj)->m_get_name().c_str());
				m_errorstr = string(buffererr);
				return false;
			}
		}
	}
	if (problem_store.outputobjnum>0) {
		for (vector<CPSGS_OutputProblem*>::iterator it_obj = m_output_problem_collection.begin(); it_obj != m_output_problem_collection.end(); it_obj++) {
			bool res = (*it_obj)->m_store_output(pFile);
			if (!res) {
				char buffererr[1024];
				sprintf(buffererr, "%s was not stored", (*it_obj)->m_get_name().c_str());
				m_errorstr = string(buffererr);
				return false;
			}
		}
	}
	fclose (pFile);
	return true;
}

bool CPSGS_Problem::m_read_problem(std::string pathtofile)
{
	size_t pos = pathtofile.find(".fpsg");
	if (pos != pathtofile.size()-5) {
		char buffererr[1024];
		sprintf(buffererr, "%s is not the PSG file", pathtofile.c_str());
		m_errorstr = string(buffererr);
		return false;
	}
	FILE * pFile;
	pFile = fopen (pathtofile.c_str(), "rb");
	if (!pFile) {
		char buffererr[1024];
		sprintf(buffererr, "cannot open %s.", pathtofile.c_str());
		m_errorstr = string(buffererr);
		return false;
	}
	problem_description problem_store;
	int64_t lsize = sizeof(problem_description);
	size_t result = fread(&problem_store , lsize, 1, pFile);
	if ((int64_t)result != lsize) {
		m_errorstr = string("problem header reading");
		return false;
	}
	m_keyprocessed = problem_store.keyprocessed==0?false:true;
	char *buffer = (char*) malloc (problem_store.namelen * sizeof(char));
	memset(buffer, 0, sizeof(char) * problem_store.namelen);
	result = fread (buffer, sizeof(char), problem_store.namelen, pFile);
	m_problem_name = string(buffer);
	delete [] buffer;
	buffer = (char*) malloc (problem_store.problemstatementlen * sizeof(char));
	memset(buffer, 0, sizeof(char) * problem_store.problemstatementlen);
	result = fread (buffer, sizeof(char), problem_store.problemstatementlen, pFile);
	m_problem_statement = string(buffer);
	delete [] buffer;
	if (problem_store.inputobjnum>0) {
		m_clear_input_objects();
		for (long i=0; i<problem_store.inputobjnum; i++) {
			CPSGS_Data_Object* thisobject = new CPSGS_Data_Object;
			if (!thisobject->m_read_object(pFile)) {
				m_clear_problem();
				m_errorstr = thisobject->m_getlasterror();
				fclose (pFile);
				return false;
			}
		}
		for (vector<CPSGS_Data_Object*>::iterator it_obj = m_input_data_obect_collection.begin(); it_obj != m_input_data_obect_collection.end(); it_obj++) {
			bool res = (*it_obj)->m_store_object(pFile);
			if (!res) {
				char buffererr[1024];
				sprintf(buffererr, "%s was not stored", (*it_obj)->m_get_name().c_str());
				m_errorstr = string(buffererr);
				return false;
			}
		}
	}
	fclose (pFile);
	return true;
}

int CPSGS_Problem::StoreStatementInTextFile(std::string pathtofile, CPSGS_Problem* thisproblem, std::string& errorstr)
{
	errorstr.clear();
	if (thisproblem->m_problem_statement.empty()) {
		errorstr = thisproblem->m_problem_name + string(" statement is empty");
		return -1;
	}
	ofstream outfile;
	outfile.open(pathtofile.c_str(), ios_base::out | ios_base::trunc);
	if(outfile.fail())	{
		errorstr = string("Cannot open ")+pathtofile;
		return -1;
	}
	outfile << thisproblem->m_problem_statement.c_str() << endl;
	outfile.close();
	return 1;
}
