module = function(file){
    env = new.env()
    sys.source(file, env , TRUE)
    class(env) = "module"
    return(env)
    }
