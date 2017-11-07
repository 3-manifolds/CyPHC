import signal
handler = signal.getsignal(signal.SIGINT)
from .phc import *
try:
    import cysignals
    _ = cysignals.init_cysignals()
except ImportError:
    pass
signal.signal(signal.SIGINT, handler)

