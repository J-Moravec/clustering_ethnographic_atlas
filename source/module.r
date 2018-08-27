


.module_init = function(){
    if(is.null(.GlobalEnv$.module_list)){
        .GlobalEnv$.module_list = new.env()
        }
    }


module = function(file){
    hash = tools::md5sum(file)
    env = .GlobalEnv$.module_list[[hash]]
    if(is.null(env)){
        env = new.env()
        sys.source(file, env, TRUE)
        class(env) = "module"
        attr(env, "hash") = hash
        .GlobalEnv$.module_list[[hash]] = env
        }
    return(env)
    }


.module_init()
