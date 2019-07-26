#pragma once

#include <time.h>
#include <vector>
#include <string>
#include <inttypes.h>

#define WRONG_VALUE -1e88

enum PSGS_SCALAR_VECTOR_TYPE {
		PSGS_SCVALVECUNKNOWN = 1,
		PSGS_SCALAR_VALUE,
		PSGS_VECTOR_VALUE,
		PSGS_EMPTY_VALUE
};

enum PSGS_FUN_OBJECT_TYPE {
		PSGS_FUNOBJUNKNOWN = 1,
		PSGS_OBJECTIVE,
		PSGS_CONSTRAINT,
		PSGS_CONSTRAINT_SLACK,
		PSGS_FUNCTION, 
		PSGS_FUNVAR
};

enum PSGS_DATA_OBJECT_TYPE {
		PSGS_DATAOBJUNKNOWN = 1,
		PSGS_MATRIX,
		PSGS_PMATRIX,
		PSGS_PARAMETER,
		PSGS_POINT,
		PSGS_VECTOR,
		PSGS_VARIABLE
};

enum PSGS_STORE_TYPE {
		PSGS_STORE_UNKNOWN = 10,
		PSGS_STORE_OBJECT_COLLECTION,
		PSGS_STORE_PROBLEM,
		PSGS_STORE_PROBLEM_OUTPUT
};

enum PSGS_SOLUTION_STATUS {
		PSGS_SOLUTION_STATUS_UNKNOWN = 20,
		PSGS_SOLUTION_STATUS_UNPROCESSED,
		PSGS_SOLUTION_STATUS_OPTIMAL,
		PSGS_SOLUTION_STATUS_FEASIBLE,
		PSGS_SOLUTION_STATUS_INFEASIBLE,
		PSGS_SOLUTION_STATUS_UNBOUNDED,
		PSGS_SOLUTION_STATUS_CALCULATED
};

enum PSGS_SOLVER_NAME {
		PSGS_SOLVER_NAME_UNKNOWN = 30,
		PSGS_SOLVER_NAME_VAN,
		PSGS_SOLVER_NAME_CAR,
		PSGS_SOLVER_NAME_TANK,
		PSGS_SOLVER_NAME_BULDOZER,
		PSGS_SOLVER_NAME_VANGRB,
		PSGS_SOLVER_NAME_CARGRB,
		PSGS_SOLVER_NAME_HELI
};

enum PSGS_PROBLEM_STRUCT_TYPE {
	UNKNOWN_PROBLEM_STRUCT_TYPE = 40,
    REGULAR_PROBLEM_STRUCT_TYPE,
    CICLYC__PROBLEM_STRUCT_TYPE
};

struct common_file_header {
	long storetype;
	long numelements;
	time_t storetime;
	long version;
	long platform;
	int64_t fullsize;
};


bool open_psgstore_file(std::string pathtofile, std::string& errorstr, common_file_header& this_header, FILE* pFile);
bool open_psgload_file(std::string pathtofile, std::string& errorstr, common_file_header& this_header, FILE* pFile);
bool set_array_fromstring(char* inputstr, long len, std::vector<std::string>& thisarray);
bool get_arrayasstring(std::vector<std::string>* thisarray, std::string& logstr);
PSGS_SOLUTION_STATUS encode_solution_status(std::string thisstatus);
std::string decode_solution_status(PSGS_SOLUTION_STATUS thisstatus);
int load_problem_statement_from_text(const std::string pathtofile, std::string& statement, std::string& problemname, std::string& errorstr);
int get_PSG_objects_from_arr(std::vector<std::string>* inp_str_arr, std::vector<std::string>& point_arr, std::vector<std::string>& vector_arr, std::vector<std::string>& matrix_arr, std::vector<std::string>& pmatrix_arr, std::vector<std::string>& added_objects_arr);
int read_matrix_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, int64_t& n_cols, std::string& errorstr);
int read_matrix_fromtext_parse(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, double** benchmark_body, double** probability_body, int64_t& n_rows, int64_t& n_cols, std::string& errorstr);
int read_pmatrix_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, int64_t& n_cols, int64_t& n_nz, std::string& errorstr);
int read_vector_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, std::string& errorstr);
int read_point_fromtext(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int64_t& n_rows, std::string& errorstr);
bool get_path_from_filename(const std::string& pathtofile, std::string& path, std::string& filename);
int read_matrix_fromtext_new(std::string pathtoobjfile, std::vector<std::string>& header_arr, double** matrix_body, int& n_rows, int& n_cols, std::string& errorstr);

namespace strutil_shell {
	char *strtok_uni(char *str, const char *delim, char **saveptr);
    std::string trim(const std::string& str);
    std::string toLower(const std::string& str);
    std::string toUpper(const std::string& str);
    std::vector<std::string> split(const std::string& str, const std::string& delimiters, bool bKeepBlanks);
	int istherestringinvector(const std::string& strinp, const std::vector<std::string>& strarr);
    char *chartrim(char *buffer, char *stripchars);
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

class  CPSGS_Fun_Object
{
public:
	CPSGS_Fun_Object(void);
	CPSGS_Fun_Object(std::string name, PSGS_FUN_OBJECT_TYPE object_type, PSGS_SCALAR_VECTOR_TYPE scalrvector_type, std::string parent_name, double value);
	~CPSGS_Fun_Object(void);
private:
	PSGS_FUN_OBJECT_TYPE m_object_type;
	PSGS_SCALAR_VECTOR_TYPE m_scalrvector_type;
	std::string m_object_name;
	std::string m_parent_name;
	double m_value;
	std::string m_errorstr;
public:
	bool operator==(CPSGS_Fun_Object& right);
	bool operator!=(CPSGS_Fun_Object& right);
	bool operator<(CPSGS_Fun_Object& right);
	bool operator>(CPSGS_Fun_Object& right);
public:
	PSGS_FUN_OBJECT_TYPE m_get_type(void) {return m_object_type;}
	std::string m_get_name(void) {return m_object_name;}
	std::string m_get_parent_name(void) {return m_parent_name;}
	double m_get_value(void) {return m_value;}
	void clearerror(void) {m_errorstr.clear();}
	std::string m_getlasterror(void) {return m_errorstr;}
	PSGS_SCALAR_VECTOR_TYPE m_get_scalrvector_type(void) {return m_scalrvector_type;}
	void m_set_object_type(PSGS_FUN_OBJECT_TYPE object_type) {m_object_type = object_type; return;}
	void m_set_object_name(std::string object_name) {m_object_name = object_name; return;}
	void m_set_parent_name(std::string parent_name) {m_parent_name = parent_name; return;}
	void m_set_value(double  value) {m_value = value;}
	void m_set_scalrvector_type(PSGS_SCALAR_VECTOR_TYPE this_type) {m_scalrvector_type = this_type;}
};

bool operator==(CPSGS_Fun_Object* left, const std::string& right);
bool operator!=(CPSGS_Fun_Object* left, const std::string& right);

class  CPSGS_Data_Object
{
public:
	CPSGS_Data_Object(void);
	CPSGS_Data_Object(std::string name, PSGS_DATA_OBJECT_TYPE object_type, std::vector<std::string>* header_arr, std::vector<int64_t>* size_arr, double* data);
	~CPSGS_Data_Object(void);
private:
	PSGS_DATA_OBJECT_TYPE m_object_type;
	std::string m_object_name;
	std::vector<std::string> m_header_arr;
	std::vector<int64_t> m_size_arr;
	int64_t m_nnz;
	std::string m_errorstr;
	double* m_data; 
public:
	bool operator==(CPSGS_Data_Object& right);
	bool operator!=(CPSGS_Data_Object& right);
	bool operator<(CPSGS_Data_Object& right);
	bool operator>(CPSGS_Data_Object& right);
public:
	PSGS_DATA_OBJECT_TYPE m_get_type(void) {return m_object_type;}
	std::string m_get_name(void) {return m_object_name;}
	void clearerror(void) {m_errorstr.clear();}
	std::string m_getlasterror(void) {return m_errorstr;}
	std::vector<int64_t> m_get_sizes(void) {return m_size_arr;}
	int64_t m_get_size(int dim) {return dim>=(int)m_size_arr.size()||dim<0?-1:m_size_arr[dim];}
	int m_get_ndims(void) {return (int)m_size_arr.size();}
	bool m_get_header_asstring(std::string& headerstr);
	std::vector<std::string>* m_get_headrarray(void) {return &m_header_arr;}
	int64_t m_get_nnz(void);
	double* m_get_data(void) {return m_data;}
	int64_t m_get_storesize(void);
	void m_set_object_type(PSGS_DATA_OBJECT_TYPE object_type) {m_object_type = object_type; return;}
	void m_set_object_name(std::string object_name) {m_object_name = object_name; return;}
	bool m_store_object(FILE* fp);
	bool m_read_object(FILE* fp);
	bool m_set_header(std::vector<std::string>* header_arr);
	bool m_set_header_fromstring(char* headerstr, long len);
	void m_set_nnz(int64_t nnz) {
		if (m_object_type==PSGS_PMATRIX) m_nnz = nnz;
	} 
	bool m_reset_data(int64_t mrows, int64_t ncols, double* data);
	bool copy(CPSGS_Data_Object* right);
	static int StoreInTextFile(std::string pathtofile, CPSGS_Data_Object* thisobject, std::string& errorstr);
};

bool operator==(CPSGS_Data_Object* left, const std::string& right);
bool operator!=(CPSGS_Data_Object* left, const std::string& right);

class  CPSGS_OutputProblem
{
public:
	CPSGS_OutputProblem(void);
	~CPSGS_OutputProblem(void);
private:
	std::string m_problem_name;
	std::string m_problem_statement;
	PSGS_SOLUTION_STATUS m_solution_status;
	double m_loading_time;
	double m_preprocessing_time;
	double m_solving_time;
	std::string m_errorstr;
	std::vector<std::string> m_outputstr_arr;
	std::vector<std::string> m_logstr_arr;
	std::vector<std::string> m_errorstr_arr;
	std::vector<std::string> m_warnings_arr;
	std::vector<CPSGS_Data_Object*> m_output_data_obect_collection;
	std::vector<CPSGS_Fun_Object*> m_output_fun_obect_collection;
public:
	bool operator==(CPSGS_OutputProblem& right) {return m_problem_name == right.m_problem_name;}
	bool operator!=(CPSGS_OutputProblem& right) {return m_problem_name != right.m_problem_name;}
	bool operator==(std::string rightname) {return m_problem_name == rightname;}
	bool operator!=(std::string rightname) {return m_problem_name != rightname;}
public:
	std::string m_get_name(void) {return m_problem_name;}
	std::string m_get_statement(void) {return m_problem_statement;}
	int m_get_statement_asvector(std::vector<std::string>& problem_statement_arr);
	void clearerror(void) {m_errorstr.clear();}
	std::string m_getlasterror(void) {return m_errorstr;}
	std::vector<std::string>* m_get_output(void) {return &m_outputstr_arr;}
	int m_set_output_from_string(std::string outputstr);
	bool m_get_output(std::string& outstr);
	double m_get_loading_time(void) {return m_loading_time;}
	double m_get_preprocessing_time(void) {return m_preprocessing_time;}
	double m_get_solving_time(void) {return m_solving_time;}
	std::vector<CPSGS_Data_Object*>* m_get_output_data_obect_collection(void) {return &m_output_data_obect_collection;}
	std::vector<CPSGS_Fun_Object*>* m_get_output_fun_obect_collection(void) {return &m_output_fun_obect_collection;}
	std::vector<std::string>* m_get_log(void) {return &m_logstr_arr;}
	bool m_get_log(std::string& logstr);
	void add_to_log(std::string str) {m_logstr_arr.push_back(str);}
	void add_to_errors(std::string str) {m_errorstr_arr.push_back(str);}
	void add_to_warnings(std::string str) {m_warnings_arr.push_back(str);}
	std::vector<std::string>* m_get_errors(void) {return &m_errorstr_arr;}
	std::vector<std::string>* m_get_warnings(void) {return &m_warnings_arr;}
	bool m_get_errors(std::string& errorstr);
	int64_t m_get_storesize(void);
	void m_add_newdataobject(CPSGS_Data_Object** newobject) {m_output_data_obect_collection.push_back(*newobject);}
	void m_add_newfunobject(CPSGS_Fun_Object** newobject) {m_output_fun_obect_collection.push_back(*newobject);}
	void m_erase_funobjects(PSGS_FUN_OBJECT_TYPE object_type);
	int m_get_funobjects(PSGS_FUN_OBJECT_TYPE object_type, std::vector<CPSGS_Fun_Object*>& this_objects_array);
	CPSGS_Fun_Object* m_get_funobject(PSGS_FUN_OBJECT_TYPE object_type, std::string object_name);
	int m_get_dataobjects(PSGS_DATA_OBJECT_TYPE object_type, std::vector<CPSGS_Data_Object*>& this_objects_array);
	PSGS_SOLUTION_STATUS m_get_solution_status(void) {return m_solution_status;}
	int m_collect_output(void);
	bool m_isin_output_data_collection(CPSGS_Data_Object* object);
	bool m_isin_output_data_collection(const std::string& objectname);
	CPSGS_Data_Object* m_get_fromoutput_data_collection(const std::string& objectname);
	bool m_isin_output_fun_collection(CPSGS_Fun_Object* object);
	bool m_isin_output_fun_collection(const std::string& objectname);
	bool m_is_objective_in_output_fun_collection(std::string& objectivename);
	void m_set_solution_status(PSGS_SOLUTION_STATUS solution_status) {m_solution_status = solution_status;}
	void m_set_solution_status(std::string thisstatus) {m_solution_status = encode_solution_status(thisstatus);}
	void m_set_loading_time(double thistime) {m_loading_time = thistime;}
	void m_set_preprocessing_time(double thistime) {m_preprocessing_time = thistime;}
	void m_set_solving_time(double thistime) {m_solving_time = thistime;}
	void m_set_name(std::string name) {m_problem_name = name;}
	void m_set_statement(std::string problem_statement) {m_problem_statement = problem_statement;}
	void m_clear_output(void) {m_outputstr_arr.clear();}
	void m_clear_log(void) {m_logstr_arr.clear();}
	void m_clear_errors(void) {m_errorstr_arr.clear();}
	void m_clear_warnins(void) {m_warnings_arr.clear();}
	bool m_store_output(FILE* fp);
	bool m_read_output(FILE* fp);
};

 bool operator==(CPSGS_OutputProblem* left, const std::string& right);
 bool operator!=(CPSGS_OutputProblem* left, const std::string& right);

class  CPSGS_Problem
{
public:
	CPSGS_Problem(void);
	~CPSGS_Problem(void);
private:
	std::string m_problem_name;
	std::string m_problem_statement;
	std::string m_strRootPath;
	std::string m_errorstr;
	bool m_keyprocessed;
	PSGS_PROBLEM_STRUCT_TYPE m_problem_struct_type;
	std::vector<CPSGS_Data_Object*> m_input_data_obect_collection;
	std::vector<CPSGS_OutputProblem*> m_output_problem_collection;
	CPSGS_OutputProblem* m_current_output_problem;
public:
	std::string m_get_name(void) {return m_problem_name;}
	std::string m_get_statement(void) {return m_problem_statement;}
	int m_get_statement_asvector(std::vector<std::string>& problem_statement_arr);
	void clearerror(void) {m_errorstr.clear();}
	std::string m_getlasterror(void) {return m_errorstr;}
	CPSGS_OutputProblem* m_get_curr_output_problem(void) {return m_current_output_problem;}
	std::string m_get_root_path(void) {return m_strRootPath;}
	void m_set_root_path(std::string path) {m_strRootPath = path;}
	std::vector<CPSGS_OutputProblem*>* m_get_output_problem_collection(void) {return &m_output_problem_collection;}
	bool m_isin_input_collection(CPSGS_Data_Object* object);
	bool m_isin_input_collection(const std::string& objectname);
	bool m_isin_input_collection(const char* pobjectname);
	bool m_is_processed(void) {return m_keyprocessed;}
	bool m_is_problem_regular(void) {return m_problem_struct_type ==  REGULAR_PROBLEM_STRUCT_TYPE;}
	bool m_is_problem_cyclic(void) {return m_problem_struct_type ==  CICLYC__PROBLEM_STRUCT_TYPE;}
	bool m_is_problem_empty(void);
	bool m_get_output_errors(std::vector<std::string>& errors_arr);
	PSGS_PROBLEM_STRUCT_TYPE m_get_problem_struct_type(void) {return m_problem_struct_type;}
	bool m_replasein_input_collection(CPSGS_Data_Object* object);
	bool m_removefrom_input_collection(CPSGS_Data_Object* object);
	CPSGS_Data_Object* m_getfrom_input_collection(const std::string& objectname);
	CPSGS_Data_Object* m_getfrom_input_collection(const std::string& objectname, PSGS_DATA_OBJECT_TYPE object_type);
	int m_get_dataobjects(PSGS_DATA_OBJECT_TYPE object_type, std::vector<CPSGS_Data_Object*>& this_objects_array);
	void m_add_newdataobject(CPSGS_Data_Object** newobject) {m_input_data_obect_collection.push_back(*newobject);}
	void m_set_name(std::string name) {m_problem_name = name;}
	void m_set_name(const char* name) {m_problem_name = std::string(name);}
	void m_set_statement(std::string statement) {m_problem_statement = statement;}
	void m_set_statement(const char* statement) {m_problem_statement = std::string(statement);}
	bool m_add_to_input_collection(CPSGS_Data_Object* object);
	void m_set_processed(bool processed) {m_keyprocessed = processed;}
	void m_set_problem_struct_type(PSGS_PROBLEM_STRUCT_TYPE thistype) {m_problem_struct_type = thistype;}
	void m_set_curr_output_problem(CPSGS_OutputProblem* thiscurroutproblem) {m_current_output_problem = thiscurroutproblem;}
	void m_set_error(std::string error) {m_errorstr = error;}
	void m_clear_input_objects(void);
	void m_clear_output_objects(void);
	void m_clear_problem(void);
	bool m_store_problem(std::string pathtofile);
	bool m_read_problem(std::string pathtofile);
	static int StoreStatementInTextFile(std::string pathtofile, CPSGS_Problem* thisproblem, std::string& errorstr);
};
