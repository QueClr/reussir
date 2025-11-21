get_property(dialect_libs GLOBAL PROPERTY MLIR_DIALECT_LIBS)
get_property(conversion_libs GLOBAL PROPERTY MLIR_CONVERSION_LIBS)
get_property(extension_libs GLOBAL PROPERTY MLIR_EXTENSION_LIBS)
get_property(translation_libs GLOBAL PROPERTY MLIR_TRANSLATION_LIBS)

add_mlir_tool(reussir-translate
  main.cpp
  DEPENDS
    MLIRReussir
    ${dialect_libs}
    ${conversion_libs}
    ${extension_libs}
    ${translation_libs}
)

target_link_libraries(reussir-translate 
  PRIVATE 
    MLIRReussir
    ${dialect_libs} 
    ${conversion_libs} 
    ${extension_libs} 
    ${translation_libs}
)
