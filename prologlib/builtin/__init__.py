from . import iso
from . import io

BUILTINS = {}
BUILTINS.update(iso.PREDICATES)
BUILTINS.update(io.PREDICATES)

def search_builtin(procedure):
    return BUILTINS.get(procedure.predicate_indicator())
