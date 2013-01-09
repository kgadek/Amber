# Things got messy, let's make them straight

function compile {
  local ROOT="apps/amber"
  local SRC="${ROOT}/c_src"
  
  local DRV="$1"
  local DRV_SRC="${SRC}/${DRV}/driver"
  local DRV_EXTRA_SRC="${DRV_SRC}/$2"
  local DRV_EXTRA_INC="${DRV_SRC}/$3"

  local CXX="g++"
  local CXXFLAGS="-pedantic -W -Wall -Wextra -Wshadow -Wformat -Winit-self -Wunused -Wfloat-equal -Wcast-qual -Wwrite-strings -Winline -Wstack-protector -Wunsafe-loop-optimizations -Wlogical-op -Wmissing-include-dirs -Wconversion -Wmissing-declarations -Wno-long-long -O2"
  local LDFLAGS="-lm -lrt -lpthread -llog4cxx -lboost_thread -lprotobuf -lboost_program_options"

  "$CXX" -o "${ROOT}/priv/${DRV}_driver" \
  "${DRV_SRC}"/*.cpp $(ls $DRV_EXTRA_SRC) "${SRC}"/amber_driver/*.cpp "${SRC}/${DRV}"/protobuf/roboclaw.pb.cc "${SRC}"/protobuf/drivermsg.pb.cc \
  -I"${DRV_SRC}" "-I${DRV_EXTRA_INC}" "-I${SRC}"/amber_driver "-I${SRC}/${DRV}"/protobuf "-I${SRC}/protobuf" \
  $CXXFLAGS $LDFLAGS
}

compile "$1" "$2" "$3"