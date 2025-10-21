# Justfile for randstr project

# Default task shows help
default:
    just --list

# Compile the project
compile:
    @echo "Compiling randstr..."
    raco make randstr/main.rkt
    raco make randstr/cli/main.rkt
    @echo "Compilation completed!"

# Run all tests
test:
    @echo "Running tests..."
    racket randstr/tests/test.rkt
    racket randstr/tests/test-extensions.rkt
    @echo "All tests passed!"

# Run main test suite
test-main:
    @echo "Running main test suite..."
    racket randstr/tests/test.rkt
    @echo "Main test suite passed!"

# Run extension tests
test-extensions:
    @echo "Running extension tests..."
    racket randstr/tests/test-extensions.rkt
    @echo "Extension tests passed!"

# Build executable file
build:
    @echo "Building executable..."
    raco exe -o randstr.exe randstr/cli/main.rkt
    @echo "Executable built as randstr.exe"

# Build executable with custom name
build-named name:
    @echo "Building executable as {{name}}..."
    raco exe -o {{name}} randstr/cli/main.rkt
    @echo "Executable built as {{name}}"

# Clean compiled files
clean:
    @echo "Cleaning compiled files..."
    rm -rf randstr/compiled
    rm -rf randstr/cli/compiled
    rm -rf tests/compiled
    @echo "Clean completed!"

# Install the package
install:
    @echo "Installing randstr package..."
    raco pkg install
    @echo "Package installed!"

# Show help
help:
    @echo "Available commands:"
    @echo "  just compile          - Compile the project"
    @echo "  just test             - Run all tests"
    @echo "  just test-main        - Run main test suite"
    @echo "  just test-extensions  - Run extension tests"
    @echo "  just build            - Build executable as run.exe"
    @echo "  just build-named NAME - Build executable with custom name"
    @echo "  just clean            - Clean compiled files"
    @echo "  just install          - Install the package"
    @echo "  just help             - Show this help"