from .phc import *
try:
    import cysignals
    cysignals.init_cysignals()
except ImportError:
    pass
