#!/bin/bash

# Script to set up Quantum ESPRESSO 7.3.1 using Micromamba and CMake
# Does not require sudo privileges
# This version uses system micromamba if available; otherwise, it installs it locally.

# Exit on error
set -e

echo "============================================="
echo "Setting up Quantum ESPRESSO 7.3.1 environment"
echo "============================================="

# Define installation directories
BASE_DIR="${HOME}/qe_tutorial_setup"
# Local micromamba installation directory (used only if system micromamba isn't found)
LOCAL_MICROMAMBA_DIR="${BASE_DIR}/micromamba"
QE_ENV_NAME="qe731"
QE_BUILD_DIR="${BASE_DIR}/qe_build"
QE_INSTALL_DIR="${BASE_DIR}/quantum-espresso-7.3.1"

# Create necessary directories
mkdir -p "${BASE_DIR}" "${QE_BUILD_DIR}" "${QE_INSTALL_DIR}" "${LOCAL_MICROMAMBA_DIR}"

# Detect OS and architecture
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

# Choose the correct micromamba download URL
MAMBA_URL=""
if [ "$OS" = "linux" ]; then
  if [ "$ARCH" = "x86_64" ]; then
    MAMBA_URL="https://micro.mamba.pm/api/micromamba/linux-64/latest"
  elif [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
    # Linux ARM/Apple Silicon under Linux
    MAMBA_URL="https://micro.mamba.pm/api/micromamba/linux-aarch64/latest"
  else
    echo "Unsupported Linux architecture: $ARCH"
    exit 1
  fi
elif [ "$OS" = "darwin" ]; then
  if [ "$ARCH" = "x86_64" ]; then
    # Intel Mac
    MAMBA_URL="https://micro.mamba.pm/api/micromamba/osx-64/latest"
  elif [ "$ARCH" = "arm64" ]; then
    # Apple Silicon (M1/M2)
    MAMBA_URL="https://micro.mamba.pm/api/micromamba/osx-arm64/latest"
  else
    echo "Unsupported macOS architecture: $ARCH"
    exit 1
  fi
else
  echo "Unsupported operating system: $OS"
  exit 1
fi

# Check if micromamba is installed system-wide
if command -v micromamba >/dev/null 2>&1; then
    echo "System micromamba found at $(command -v micromamba)."
    MICROMAMBA_EXE=$(command -v micromamba)
else
    echo "System micromamba not found. Installing micromamba locally..."
    cd "${LOCAL_MICROMAMBA_DIR}"

    # Download and extract micromamba for the detected platform
    curl -Ls "${MAMBA_URL}" | tar -xvj bin/micromamba

    MICROMAMBA_EXE="${LOCAL_MICROMAMBA_DIR}/bin/micromamba"
    ${MICROMAMBA_EXE} shell init -s bash -r "${LOCAL_MICROMAMBA_DIR}"
    # For a local install, set the root prefix to the local directory.
    export MAMBA_ROOT_PREFIX="${LOCAL_MICROMAMBA_DIR}"
fi

# Initialize micromamba for this session
eval "$(${MICROMAMBA_EXE} shell hook -s bash)"

# Create a QE environment with all required dependencies
echo "Creating Quantum ESPRESSO environment with dependencies..."
micromamba create -n ${QE_ENV_NAME} -c conda-forge -y \
    gfortran compilers cmake=3.31.6 ninja \
    openmpi mpi4py \
    fftw libxc scalapack hdf5 zlib blas lapack openblas \
    wget git python

# Activate the QE environment
micromamba activate ${QE_ENV_NAME}

# Download Quantum ESPRESSO 7.3.1
echo "Downloading Quantum ESPRESSO 7.3.1..."
cd "${QE_BUILD_DIR}"
wget https://github.com/QEF/q-e/archive/refs/tags/qe-7.3.1.tar.gz
tar -xzf qe-7.3.1.tar.gz
cd q-e-qe-7.3.1

# Get the environment prefix from micromamba
MAMBA_ENV_PREFIX=$(micromamba info -n ${QE_ENV_NAME} | grep "env location" | awk -F': ' '{print $2}')

# Create build directory for CMake
mkdir -p build
cd build

# Configure with CMake
echo "Configuring Quantum ESPRESSO with CMake..."
cmake .. \
    -DCMAKE_INSTALL_PREFIX="${QE_INSTALL_DIR}" \
    -DCMAKE_PREFIX_PATH="${MAMBA_ENV_PREFIX}" \
    -DCMAKE_C_COMPILER=mpicc \
    -DCMAKE_Fortran_COMPILER=mpif90 \
    -DQE_ENABLE_MPI=ON \
    -DQE_ENABLE_OPENMP=ON \
    -DQE_ENABLE_SCALAPACK=ON \
    -DQE_ENABLE_HDF5=ON \
    -DQE_ENABLE_LIBXC=OFF

# Build and install
echo "Building Quantum ESPRESSO (this may take a while)..."
make -j$(nproc)
make install

# Create an activation script for the QE environment
cat > "${BASE_DIR}/activate_qe.sh" << EOF
#!/bin/bash

# Source micromamba (use system micromamba if available, otherwise use local installation)
if command -v micromamba >/dev/null 2>&1; then
    eval "\$(micromamba shell hook -s bash)"
else
    export MAMBA_ROOT_PREFIX="${LOCAL_MICROMAMBA_DIR}"
    eval "\$(${LOCAL_MICROMAMBA_DIR}/bin/micromamba shell hook -s bash)"
fi

# Activate the QE environment
micromamba activate ${QE_ENV_NAME}

# Add QE binaries to PATH and set library path
MAMBA_ENV_PREFIX=$(micromamba info -n ${QE_ENV_NAME} | grep "env location" | awk -F': ' '{print $2}')
export PATH="${QE_INSTALL_DIR}/bin:\$PATH"
export LD_LIBRARY_PATH="\${MAMBA_ENV_PREFIX}/lib:\$LD_LIBRARY_PATH"

echo "Quantum ESPRESSO 7.3.1 environment activated!"
EOF

chmod +x "${BASE_DIR}/activate_qe.sh"

# Clean up
echo "Cleaning up..."
rm "${QE_BUILD_DIR}/qe-7.3.1.tar.gz"

echo "======================================================="
echo "Quantum ESPRESSO 7.3.1 has been installed successfully!"
echo "======================================================="
echo ""
echo "To activate the Quantum ESPRESSO environment, run:"
echo "source ${BASE_DIR}/activate_qe.sh"
echo ""
echo "To verify installation, after activation run: 'pw.x -h'"
echo "======================================================="
