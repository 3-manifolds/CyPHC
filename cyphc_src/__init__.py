import signal
handler = signal.get_signal(signal.SIGINT)
from .phc import *
try:
    import cysignals
    _ = cysignals.init_cysignals()
except ImportError:
    pass
signal.signal(signal.SIGINT, handler)

