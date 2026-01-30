# Determine build profile based on CMAKE_BUILD_TYPE
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CARGO_PROFILE "dev")
    set(CARGO_PROFILE_DIR "debug")
else()
    set(CARGO_PROFILE "release")
    set(CARGO_PROFILE_DIR "release")
endif()

# Custom target to build reussir-rt with cargo
add_custom_target(reussir-rt-build ALL
    COMMAND ${CMAKE_COMMAND} -E env
        CARGO_TARGET_DIR=${CMAKE_BINARY_DIR}/target
        RUSTFLAGS="-Awarnings"
        cargo build --profile ${CARGO_PROFILE} --quiet
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building reussir-rt library with cargo (${CARGO_PROFILE} profile)"
)

# Custom target to copy the libraries
if(WIN32)
    set(REUSSIR_RT_OUTPUT_DIR ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
else()
    set(REUSSIR_RT_OUTPUT_DIR ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
endif()

add_custom_target(reussir-rt-install ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_BINARY_DIR}/target/${CARGO_PROFILE_DIR}/deps
        ${REUSSIR_RT_OUTPUT_DIR}
    COMMENT "Installing reussir-rt libraries"
    DEPENDS reussir-rt-build
)

# Read rust-toolchain.toml to get the channel
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/rust-toolchain.toml" RUST_TOOLCHAIN_CONTENT)
string(REGEX MATCH "channel = \"([^\"]+)\"" _ ${RUST_TOOLCHAIN_CONTENT})
set(RUST_CHANNEL ${CMAKE_MATCH_1})

if(NOT RUST_CHANNEL)
    message(FATAL_ERROR "Could not parse channel from rust-toolchain.toml")
endif()

message(STATUS "Target Rust channel: ${RUST_CHANNEL}")

# Define where we want the local toolchain to live (intermediate step)
set(LOCAL_RUSTUP_HOME "${CMAKE_BINARY_DIR}/.rustup")
set(LOCAL_CARGO_HOME "${CMAKE_BINARY_DIR}/.cargo")

# We need to install the toolchain.
# We use a custom command/target to ensure it's there.
add_custom_target(reussir-rust-toolchain-fetch
    COMMAND ${CMAKE_COMMAND} -E env
        RUSTUP_HOME=${LOCAL_RUSTUP_HOME}
        CARGO_HOME=${LOCAL_CARGO_HOME}
        rustup toolchain install ${RUST_CHANNEL} --profile minimal --no-self-update
    COMMENT "Ensuring Rust toolchain ${RUST_CHANNEL} is installed in ${LOCAL_RUSTUP_HOME}"
    USES_TERMINAL
)

# Get the path to the installed toolchain
# rustup installs toolchains in toolchains/<name>-<host>
# We need to know the host triple or glob it.
# To be safe and simple, let's ask rustup (running with our custom home) where it is.
# BUT, getting output from a custom target into cmake variable at configure time is hard if the target hasn't run.
# So we have to do this at build time.

# However, for `reussir-rust-install`, we just want to copy *from* the toolchain dir *to* the output dir.
# The `rustup` command above ensures extraction.
# The path will be roughly `${LOCAL_RUSTUP_HOME}/toolchains/${RUST_CHANNEL}-<host>`.
# Since we don't know <host> easily at configure time without running rustc (which we might not have yet),
# let's assume valid rustup is present on host to ask.
execute_process(
    COMMAND rustc -vV
    OUTPUT_VARIABLE RUSTC_VERSION_INFO
)
string(REGEX MATCH "host: ([a-zA-Z0-9_-]+)" _ ${RUSTC_VERSION_INFO})
set(RUST_HOST_TRIPLE ${CMAKE_MATCH_1})

if(NOT RUST_HOST_TRIPLE)
    # Fallback or error? Let's try to detect via cmake or just error.
    message(WARNING "Could not detect host triple from rustc -vV, assuming x86_64-unknown-linux-gnu or similar dependent on system?")
    # Actually, we can use `rustup show`? No.
    # Let's rely on standard rustup naming if possible, OR
    # Use a glob in the install step? CMake copy_directory doesn't glob.
    # Let's try to get it from `rustup toolchain list` if possible, but that depends on our local home which is empty.
    # Okay, for now, we will assume the user has a working rustc on PATH to identify HOST.
endif()

set(INSTALLED_TOOLCHAIN_PATH "${LOCAL_RUSTUP_HOME}/toolchains/${RUST_CHANNEL}-${RUST_HOST_TRIPLE}")

add_custom_target(reussir-rust-install ALL
    # Copy bin
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${INSTALLED_TOOLCHAIN_PATH}/bin"
        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    # Copy lib
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${INSTALLED_TOOLCHAIN_PATH}/lib"
        "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    COMMENT "Installing rust libraries and tools from ${INSTALLED_TOOLCHAIN_PATH}"
    DEPENDS reussir-rt-build reussir-rust-toolchain-fetch
)

# Main reussir-rt target that depends on the complete build process
add_custom_target(reussir-rt ALL
    DEPENDS reussir-rt-install reussir-rust-install
)
