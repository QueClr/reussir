import lit.formats
import os
import sys

config.name = 'Reussir'
config.test_format = lit.formats.ShTest(True)

config.suffixes = ['.mlir', '.rr', '.repl']

config.test_source_root = os.path.dirname(__file__)
config.test_exec_root = os.path.join(config.test_output_root, 'test')

config.substitutions.append((r'%reussir-opt',
                             os.path.join(config.binary_path, 'reussir-opt')))
config.substitutions.append((r'%reussir-translate',
                             os.path.join(config.binary_path, 'reussir-translate')))

# Add C compiler substitution using CMake's C compiler
config.substitutions.append((r'%cc', config.cc_path))

config.substitutions.append((r'%FileCheck', config.filecheck_path))
config.substitutions.append((r'%not', config.not_path))
config.substitutions.append((r'%opt', config.opt_path))
config.substitutions.append((r'%library_path', config.library_path))
config.substitutions.append((r'%llc', config.llc_path))
config.substitutions.append((r'%extra_sys_libs', config.extra_sys_libs))
config.substitutions.append((r'%lli', config.lli_path))
config.substitutions.append((r'%rpath_flag', config.rpath_flag))
config.substitutions.append((r'%reussir-elab', config.reussir_elab_path))
config.substitutions.append((r'%reussir-compiler', config.reussir_compiler_path))
config.substitutions.append((r'%reussir-repl', config.reussir_repl_path))
config.substitutions.append((r'%reussir-parser', config.reussir_parser_path))

# Platform features for REQUIRES/UNSUPPORTED directives
if sys.platform == 'win32':
    config.available_features.add('system-windows')
    config.substitutions.append((r'%reussir_rt', 'reussir_rt.dll'))
    # Ensure build output directory is in PATH for DLL resolution
    config.environment['PATH'] = config.library_path + os.pathsep + os.environ.get('PATH', '')
elif sys.platform == 'darwin':
    config.available_features.add('system-darwin')
    config.substitutions.append((r'%reussir_rt', 'libreussir_rt.dylib'))
else:
    config.available_features.add('system-linux')
    config.substitutions.append((r'%reussir_rt', 'libreussir_rt.so'))
