#!/usr/bin/env python
# ------------------------------------------------------------------ #
# PREPARE-SIMULATIONS.py
# ------------------------------------------------------------------ #
# Generates one Lisp file for every point in the hyperparameter
# space. The simulations can then be run in parallel on multi-core
# architectures
# ------------------------------------------------------------------ #
#
#   (c) 2018, Andrea Stocco
#             University of Washington,
#             Seattle, WA 98195
#             Email: stocco@uw.edu
#
# ------------------------------------------------------------------ #

import numpy as np
import sys
import string
import functools
import operator

LISP_INTRO = """
(load "/projects/actr/actr7/load-act-r.lisp")
(load "2afc-device.lisp")
(load "2afc-model.lisp")
(load "2afc-simulations.lisp")
"""
LISP_SIMS = """
(simulate %d :params '%s :start %d :filename "%s")
"""

LISP_END = """
;;; Quit simulations
(quit)
"""


# --------------------------------------------------------------------
# COMBINATORIAL FUNCTIONS
# --------------------------------------------------------------------

def cmbn(lst1, lst2):
    """Generates the cartesian product of all elements of two lists"""
    res = []
    for a in lst1:
        for b in lst2:
            partial = []
            if isinstance(a, list) and isinstance(b, list):
                partial = a + b

            elif isinstance(a, list) and not isinstance(b, list):
                partial = a + [b]

            elif not isinstance(a, list) and isinstance(b, list):
                partial = [a] + b

            elif not isinstance(a, list) and not isinstance(b, list):
                partial = [a, b]
            
            res.append(partial)
    return res


def combinations(lst):
    """Returns the combinations of all the lists in LST"""
    if len(lst) > 0:
        if len(lst) == 1:
            return [[x] for x in lst[0]]
        else:
            res = lst[0]
            for axis in lst[1:]:
                res = cmbn(res, axis)
            return res
    else:
        return []


# --------------------------------------------------------------------
# ParamRange
# ---------------------------------------------------------------------
# Defines a parameter (i.e., a dimension in hyperspace) and its range
# --------------------------------------------------------------------
    
class ParamRange():
    """Defines a parameter range in abstract terms"""
    def __init__(self, name, start, end, step):
        if self.is_param_name(name)        \
           and self.is_param_value(start)  \
           and self.is_param_value(end)    \
           and self.is_param_value(step)   \
           and float(end) >= float(start):
            #print("Creating...")
            self.name = name
            self.start = float(start)
            self.end = float(end)
            self.step = float(step)

        else:
            pass
        
    @property
    def name(self):
        return self._name

    @name.setter
    def name(self, string):
        if self.is_param_name(string):
            self._name = string

    @property
    def start(self):
        return self._start

    @start.setter
    def start(self, val):
        if self.is_param_value(val):
            self._start = val


    @property
    def end(self):
        return self._end

    
    @end.setter
    def end(self, val):
        if self.is_param_value(val):
            self._end = val


    @property
    def step(self):
        return self._step

    @step.setter
    def step(self, val):
        if self.is_param_value(val):
            self._step = val
    
    def is_param_name(self, string):
        if isinstance(string, str) \
           and len(string) > 1    \
           and string[0:1] is ":" :

            return True
        else:
            return False


    def is_param_value(self, string):
        try: 
            float(string)
            return True
        except ValueError:
            return False

    def __repr__(self):
        return "<PR: '%s' (%.3f, %.3f, %.3f)>" % (self.name, self.start, self.end, self.step)

    def __str__(self):
        return self.__repr__()
    
    def expand(self):
        """Returns the range as a list"""
        #k = self.start
        res = [self.start]
        k = self.start + self.step
        if self.step > 0:
            while k <= self.end:
                res.append(k)
                k += self.step
        return list(res)

    
# --------------------------------------------------------------------
# The Hyper Point
# --------------------------------------------------------------------
# The Hyperpoint is the core of a set of simulations. Essentially,
# the code that is generated will simulate and run N subjects that are
# "clones" of each other in a parameter sense --- that is, they all
# share the same parameter values, as specified by the hyperpoint
# dimensions.
# The hyperpoint class contains functions to handle and sort,
# dimensions, as well as functions to generate corresponding Lisp
# code.
# --------------------------------------------------------------------

class Sanitazable():
    def sanitize(self, name):
        """Removes non-printable letters from name string"""
        return "".join([x.lower() for x in name if x in string.ascii_letters or x in string.digits or x in "_-+=."])


class HyperPoint(Sanitazable):
    """Hyperpoint in parameter space"""
    def __init__(self,
                 parameters,
                 values,
                 num = 100,
                 start = 0,
                 model="hp_simulations"):
        """
        Initializes a hyperspace. Needs a list of params (dimensions)
        """
        #print([parameters, values])
        self._internal = dict(zip(parameters, values))
        self.num = num
        self.start = start
        self.model = model

        
    def add_dimension(self, name, value):
        """Adds a dimension (parameter) to the hyperpoint"""
        if name not in self._internal.keys():
            self._internal[name] = value

    @property
    def num(self):
        """Number of simulations per hyperpoint"""
        return self._num

    @num.setter
    def num(self, val):
        """
        Sets the number of simulations per hyperpoint. 
        Needs to be an int
        """
        if isinstance(val, int):
            self._num = val

    @property
    def start(self):
        """The starting point of the IDX counter"""
        return self._start

    @start.setter
    def start(self, val):
        """
        Sets the starting point of the ID counter. 
        Needs to be an int
        """
        if isinstance(val, int):
            self._start = val

    @property
    def model(self):
        """The model name (a prefix)"""
        return self._model

    @model.setter
    def model(self, name):
        """Sets the model name (the filename prefix)"""
        if isinstance(name, str):
            self._model = name
            
    def get_dimensions(self):
        """Returns the names of all dimensions in alphabetical order"""
        res = list(self._internal.keys())
        res.sort()
        return res

    
    def get_dimension_value(self, name):
        """Returns the value of a specific dimension (i.e., param)"""
        if name in list(self._internal.keys()):
            return self._internal[name]

        
    def __repr__(self):
        """String representation of the hyperpoint (in Lisp style)""" 
        return self.lisp_representation()

    
    def __str__(self):
        """String representation of the hyperpoint (in Lisp style)"""
        return self.__repr__()
        

    @property
    def filename(self):
        """Generates the (sanitized) name of an output file"""
        params = list(self._internal.keys())
        params.sort()
        fname = self.model + "_"
        for p in params[:-1]:
            fname += ("%s_%.3f_" % (self.sanitize(p),
                                    self._internal[p]))
        fname += ("%s_%.3f" % (self.sanitize(params[-1]),
                                self._internal[params[-1]]))
        fname += ".txt"
        return fname


    def lisp_representation(self):
        """
        Returns a string representing the hyperpoint in Lisp-like
        notation, e.g. '((x val) (y val) (z val))
        """
        string = "("
        params = list(self._internal.keys())
        params.sort()
        for k in params[:-1]:
            v = self._internal[k]  # The value
            string += "(%s %.3f) " % (k, v)
        
        string += "(%s %.3f))" % (params[-1], self._internal[params[-1]])
        return string


    @property
    def lisp_code(self):
        """
        Generates the lisp code that examines model performance in
        this hyperpoint of the parameter space
        """
        return LISP_SIMS % (self.num, self, self.start, self.filename)

    
    def belongs_to_hyperplane(self, hyperplane):
        """
        A point belongs to an hyperplane if it contains all the 
        dimensions and values of the plane.
        """
        for p in hyperplane.get_dimensions():
            if self.get_dimension_value(p) != hyperplane.get_dimension_value(p):
                return False
        return True

# --------------------------------------------------------------------
# HyperSpace
# --------------------------------------------------------------------
# A hyperspace is an abstract representation of a collection of
# HyperPoints. The HyperSpace class supports code generations to
# simulate all the points in the hyperspace, as well as code to divide
# the HyperSpace into subregions.
# --------------------------------------------------------------------
            
    
class HyperSpace(Sanitazable):
    """Hyper parameter space"""
    def __init__(self, plist, num = 100, start = 0, model = "simulations"):
        self.params = plist
        self.num = num
        self.start = start
        self.model = model

    @property
    def params(self):
        """The list of parameters of the hyperspace"""
        return self._params

    @params.setter
    def params(self, lst):
        """
        Sets the list of parameters. 
        only 'ParamRange' objects are accepted
        """
        P = [x for x in lst if isinstance(x, ParamRange)]
        self._params = P


    def get_param(self, name):
        """Returns the param corresponding to the given name"""
        for p in self.params:
            if p.name == name:
                return p

        return None
        
    def set_dimension(self, dimension):
        """Adds or redefines a new dimension of the hyperspace"""  
        if isinstance(dimension, ParamRange):
            if dimension.name not in [x.name for x in self.params]:
                self.params.append(dimension)
            else:
                P = [x for x in self.params if x.name != dimension.name]
                P.append(dimension)
                self.params = P
        
    @property
    def num(self):
        """The number of simulations"""
        return self._num

    @num.setter
    def num(self, val):
        """
        Sets the number of simulations. 
        Only int vals are accepted
        """
        if isinstance(val, int):
            self._num = val

    @property
    def start(self):
        """Returns the starting ID for a simulation"""
        return self._start

    @start.setter
    def start(self, val):
        """Sets the starting ID for a simulation"""
        if isinstance(val, int):
            self._start = val
            
    @property
    def model(self):
        """The model name"""
        return self._model

    @model.setter
    def model(self, name):
        """Sets the model name"""
        if isinstance(name, str):
            self._model = name

    @property
    def points(self):
        """Returns this hyperspace as a list of hyperpoints"""
        names = [p.name for p in self.params]
        values = [p.expand() for p in self.params]
        points = combinations(values)
        P = [HyperPoint(names, coordinates, num = self.num) for coordinates in points]

        # Give each point a unique start 
        j = self.start
        for p in P:
            p.start = j
            j += self.num

        return P
    
    @property
    def size(self):
        """Returns the number of points in the hyperspace"""
        sizes = [len(x.expand()) for x in self.params]
        return functools.reduce(operator.mul, sizes) 

    # Here we should include a function to chop
    # the space into N subspaces, cut somehow.
    # Or we should include a function to "cut across"
    # N possible dimensions, simulating across all
    # the others
    #
    # e.g. - cut_across([param1, param2, ..., paramN])
    #  --> N smaller hyperspaces
    def cut_across(self, param_name):
        """
        Returns a series of hyperspaces across the values 
        of given parameter. Useful to create multi-thread 
        simulations.
        """
        p = self.get_param(param_name)
        vals = p.expand()
        subparams = [ParamRange(param_name, x, x, 0.0) for x in vals]
        subspaces = [HyperSpace(self.params, self.num, self.start, self.model) for x in subparams]
        for p, space in zip(subparams, subspaces):
            space.set_dimension(p)
            space.model = self.model + "_%s_%.3f" % (p.name, p.start)

        return subspaces
        
    
    def divide_into(n):
        """
        Attempts to devide the parameter space into N subspaces
        (current unimplemented)
        """
        pass

    @property
    def code(self):
        """Returns the code that explores the entire hyperspace"""
        res = LISP_INTRO
        for p in self.points:
            res += p.lisp_code
        res += LISP_END
        return res

    @property
    def filename(self):
        """Generates its own filename"""
        return self.sanitize(self.model) + ".lisp"

    def save_code(self):
        """Saves the code on a file named after its own space"""
        fout = open(self.filename, 'w')
        fout.write(self.code)
        fout.flush()
        fout.close()
    
    def __repr__(self):
        """A condensed string representation of the hyperspace""" 
        return "{HS(%d)%s}" % (self.size, self.params)

    def __str__(self):
        """A condensed string representation of the hyperspace"""
        return self.__repr__()


# --------------------------------------------------------------------
# load_params
# --------------------------------------------------------------------
# A simple function that loads parameters from a text file
# --------------------------------------------------------------------

def load_params(filename="params.txt"):
    """Loads the param specification and creates the hyperspace"""
    fin = open(filename, 'r')
    params = []
    for n, l in enumerate(fin):
        #print("%d, '%s'" % (n, l))
        # Remove comments that start with '#';
        var = l
        if "#" in var:
            var = var[0:var.index("#")]
        var = var.split()
        var = [x.strip() for x in var]
        if len(var) == 4:
            candidate = ParamRange(var[0], var[1], var[2], var[3])
            if candidate is None:
                print("Error in line %d: ``%s''', no parameter created" % (n, l))
            else:
                params.append(candidate)
        elif len(var) > 0: 
            print("Not a valid param definition: Line %d, ``%s''', no parameter created" % (n, l))
    #print(params)
    return [x for x in params if x is not None]

    
# --------------------------------------------------------------------
# TEST
# --------------------------------------------------------------------
k1 = ParamRange(":A", 0, 1, 0.1)
k2 = ParamRange(":B", -2, 2, 0.5)
h1 = HyperSpace([k1])
p1 = h1.points
h2 = HyperSpace([k1, k2])
p2 = h2.points

# --------------------------------------------------------------------
# When executing it as a script
# --------------------------------------------------------------------

HELP = """
Usage:
   %s <paramfile>

Where <paramfile> is a file describing the parameter space
with the notation:

   param1 start end step
   param2 start end step
   ...
   paramZ start end step 

(comments are accepted with '#' mark)
"""

if __name__ == "__main__":
    if len(sys.argv) == 2:
        params = load_params(sys.argv[1])
        #print(params)
        if len(params) > 0:
            hs = HyperSpace(params)
            #print(hs)
            hspaces = [hs]
            if len(params)> 1:
                hspaces = hs.cut_across(params[0].name)
            for j in hspaces:
                j.save_code()

    else:
        print(HELP % sys.argv[0])
        
