#include <stdio.h>
#include <stdlib.h>

/*
  Los objetos van a estar enlazados entre sí, de tal manera que la máquina virtual tiene una
  referencia al primer objeto creado. Así mismo, el entorno tiene referencias a los objetos
  raíz del cómputo.
*/

typedef enum {
  SCM_INT,
  SCM_NIL,
  SCM_PAIR
} ScmType;

typedef struct ScmObject {
  ScmType type;
  union {
    int i;
    struct {
      struct ScmObject* car;
      struct ScmObject* cdr;
    };
  };
} ScmObject;

typedef struct EnvironmentVariable {
  struct EnvironmentVariable* next;
  char* id;
} EnvironmentVariable;

typedef struct EnvironmentValue {
  struct EnvironmentValue* next;
  ScmObject* obj;
} EnvironmentValue;

typedef struct ScmEnvironment {
  struct ScmEnvironment* next;
  EnvironmentVariable* vars;
  EnvironmentValues* vals;
} ScmEnvironment;

EnvironmentValue* environmentLookup(ScmEnvironment* env, char* id) {
  assert(env != NULL, "environmentLookup: NULL environment");
  assert(id != NULL, "environmentLookup: NULL identifier");
  
  ScmEnvironment*      curr_env;
  EnvironmentVariable* curr_var;
  EnvironmentValue*    curr_val;

  // iterate over the list of environments
  for (curr_env = env; curr_env != NULL; curr_env = curr_env->next) {
    // iterate over the variables and values of the current environment
    for (curr_var = curr_env->vars, curr_val = curr_env->vals;
	 curr_var != NULL;
	 curr_var = curr_var->next, curr_val = curr_val->next) {

      // if the current variable identifier is the same as id
      // return the current value
      if (strcmp(curr_var->id, id) == 0) {
	return curr_val;
      }
    }
  }
  // if the execution reaches this point it means that there wasn't any variable in env
  // with the identifier id
  return NULL;
}

ScmEnvironment* environmentExtend(ScmEnvironment* next,
				  EnvironmentVariable* vars,
				  EnvironmentValues* vals) {
  ScmEnvironment* env = malloc(sizeof(ScmEnvironment));
  
  env->next = next;
  env->vars = vars;
  env->vals = vals;

  return env;
}

ScmEnvironment* environmentPush(ScmEnvironment* env,
				char* id,
				ScmObject* obj) {
  assert(env != NULL, "environmentPush: NULL environment");
  assert(id != NULL, "environmentPush: NULL identifier");
  assert(obj != NULL, "environmentPush: NULL object");
  
  EnvironmentVariable* var = malloc(sizeof(EnvironmentVariable));
  EnvironmentValue* val = malloc(sizeof(EnvironmentValue));
  
  var->next = env->vars; var->id = id;
  val->next = env->vals; val->obj = obj;
  env->vars = var;
  env->vals = val;
  
  return env;
}

typedef struct VirtualMachine {
  
} VirtualMachine;
