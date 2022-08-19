# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkupthrift.types import (
    ThriftThriftFileEntityXRefLocations,
    ThriftThriftResolveLocation,
    ThriftThriftEntityLocation,
)


class CodemarkupThriftThriftFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.thrift.ThriftFileEntityXRefLocations.4 { { } }", ThriftThriftFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupThriftThriftFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupThriftThriftResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.thrift.ThriftResolveLocation.4 { { } }", ThriftThriftResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupThriftThriftResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupThriftThriftEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.thrift.ThriftEntityLocation.4 { { } }", ThriftThriftEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupThriftThriftEntityLocation":
    raise Exception("this function can only be called from @angle_query")


