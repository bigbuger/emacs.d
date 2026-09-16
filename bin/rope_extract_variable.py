#!/usr/bin/env python3
import sys

from rope.refactor import extract
from rope.base import libutils
import rope.base.project


root = sys.argv[1]
file = sys.argv[2]
start = int(sys.argv[3])
end = int(sys.argv[4])
var_name = sys.argv[5]
print(sys.argv)

project = rope.base.project.Project(root)
resource = libutils.path_to_resource(project, file)
project.validate(resource)

extractor = extract.ExtractVariable(project, resource, start, end)
changes = extractor.get_changes(var_name)
project.do(changes)

