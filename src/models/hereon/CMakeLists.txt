add_library(fabm_models_hereon OBJECT
            hereon_model_library.F90
            light/light.F90
	    omexdia_p/omexdia_p.F90
           )

add_dependencies(fabm_models_hereon fabm_base)
