#!/usr/bin/env python3
from rope.base.resources import File, Folder
import sys

import rope.base.exceptions
import rope.base.project
from rope.base import libutils
from rope.refactor import extract
from rope.refactor import inline
from rope.refactor.move import MoveGlobal, MoveMethod, MoveModule, create_move
from rope.refactor.extract import ExtractMethod
from rope.refactor.extract import ExtractVariable

print("rope-cli start: ")
print(sys.argv)
action = sys.argv[1]
root = sys.argv[2]
file = sys.argv[3]


project = rope.base.project.Project(root)
resource = libutils.path_to_resource(project, file)
project.validate(resource)


def extract_variable():
    start = int(sys.argv[4])
    end = int(sys.argv[5])
    var_name = sys.argv[6]
    
    extractor: ExtractVariable = extract.ExtractVariable(project, resource, start, end)
    changes = extractor.get_changes(extracted_name=var_name)
    project.do(changes)

def extract_method():
    start = int(sys.argv[4])
    end = int(sys.argv[5])
    method_name = sys.argv[6]
    
    extractor: ExtractMethod = extract.ExtractMethod(project, resource, start, end_offset=end)
    changes = extractor.get_changes(method_name)
    project.do(changes)
    
def inline_method():
    offset = int(sys.argv[4])
    inlineMethod = inline.InlineMethod(project, resource, offset)
    changes = inlineMethod.get_changes()
    project.do(changes)
    
def move():
    offset = int(sys.argv[4])
    target_file = sys.argv[5]
    mover = create_move(project, resource, offset)
    target_resource = project.get_resource(resource_name=target_file)
    project.validate(target_resource)
    project.do(mover.get_changes(target_resource))

def move_module():
    target_file = sys.argv[4]
    mover:  MoveModule = create_move(project, resource)
    target_resource = project.get_resource(resource_name=target_file)
    project.validate(target_resource)
    project.do(mover.get_changes(target_resource))


try:
    if action == "extract_variable":
        extract_variable()
    elif action == "extract_method":
        extract_method()
    elif action == "inline_method":
        inline_method()
    elif action == "move":
        move()
    elif action == "move_module":
        move_module()
except rope.base.exceptions.RopeError as e:
    print(f"Rope exception: {e}")
    exit(1)
