import inspect
from functools import wraps
from plotnine.stats.stat import stat

class instancemethod:
    def __init__(self, func):
        self.func = func

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self._class_view()

        @wraps(self.func)
        def bound(*args, **kwargs):
            return self.func(*args, **kwargs)

        bound.__signature__ = self._method_signature()
        return bound

    def _method_signature(self):
        sig = inspect.signature(self.func)
        params = list(sig.parameters.values())

        self_param = inspect.Parameter(
            "self",
            inspect.Parameter.POSITIONAL_OR_KEYWORD
        )

        return inspect.Signature([self_param, *params])

    def _class_view(self):
        @wraps(self.func)
        def method(*args, **kwargs):
            return self.func(*args, **kwargs)

        method.__signature__ = self._method_signature()
        return method


def qstat(compute_group, **kwargs):
    return type(
        "_stat_temp",
        (stat,),
        {"compute_group": instancemethod(compute_group)},
        **kwargs
    )


def qstat_panel(compute_panel, **kwargs):
    return type(
        "_stat_temp",
        (stat,),
        {"compute_panel": instancemethod(compute_panel)},
        **kwargs
    )


def qstat_layer(compute_layer, **kwargs):
    return type(
        "_stat_temp",
        (stat,),
        {"compute_layer": instancemethod(compute_layer)},
        **kwargs
    )