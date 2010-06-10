import rlcompleter, readline, atexit, os
readline.parse_and_bind('tab: complete')
readline.parse_and_bind("set input-meta on")
readline.parse_and_bind("set convert-meta off")
readline.parse_and_bind("set output-meta on")
histfaile = os.path.join(os.environ['HOME'], 'pythonhistory')
try:
    readline.read_history_file(histfaile)
except IOError:
    pass
atexit.register(readline.write_history_file, histfaile)
del os, histfaile, readline, rlcompleter, atexit

import inspect
igd = inspect.getdoc
igs = inspect.getsource
igf = inspect.getfile
igsl = inspect.getsourcelines
