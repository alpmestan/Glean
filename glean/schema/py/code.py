# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.code.types import (
    EntityLanguageLSIF,
    EntityLanguage,
)


class CodeEntityLanguageLSIF(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"code.EntityLanguageLSIF.24 { { } }", EntityLanguageLSIF

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodeEntityLanguageLSIF":
    raise Exception("this function can only be called from @angle_query")

class CodeEntityLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"code.EntityLanguage.24 { { } }", EntityLanguage

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodeEntityLanguage":
    raise Exception("this function can only be called from @angle_query")


