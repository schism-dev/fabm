add_library(fabm_models_vims OBJECT
            vims_model_library.F90
            cosine/cosine.F90
            cosine/fabm_cosine_misc.F90
           )

add_dependencies(fabm_models_vims fabm_base)

#-----------------------------------------------------------------------
# Copyright (C) 2014 - Karsten Bolding and Jorn Bruggeman (BB)         !
#-----------------------------------------------------------------------
