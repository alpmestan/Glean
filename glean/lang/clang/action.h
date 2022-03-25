/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/cpp/glean.h"

#include <string>
#include <chrono>
#include <memory>
#include <functional>
#include <iostream>

#include <folly/Optional.h>

#if FACEBOOK
namespace facebook::logger {
  class GleanClangIndexerLogger;
}
#endif

namespace facebook {
namespace glean {
namespace clangx {

#if FACEBOOK
using Logger = facebook::logger::GleanClangIndexerLogger;
#else
class Logger {
public:
  Logger() { }
  Logger& setTask(const std::string& task) {
    std::cout << "logger task set to: " << task << std::endl;
    return (*this);
  }
  Logger& setRequest(const std::string& task) {
    std::cout << "logger request set to: " << task << std::endl;
    return (*this);
  }
  Logger& setRepo(const std::string& repo) {
    std::cout << "logger repo set to: " << repo << std::endl;
    return (*this);
  }
  Logger& setRevision(const std::string& repo_hash) {
    std::cout << "logger revision set to: " << repo_hash << std::endl;
    return (*this);
  }
  Logger& setProcess(const uint32_t& worker_index) {
    std::cout << "logger process set to: " << worker_index << std::endl;
    return (*this);
  }
  Logger& setCommand(const std::string& name) {
    std::cout << "logger command set to: " << name << std::endl;
    return (*this);
  }
  Logger& setOrigin(const std::string& origin) {
    std::cout << "logger origin set to: " << origin << std::endl;
    return (*this);
  }
  Logger& setSubdir(const std::string& cwd_subdir) {
    std::cout << "logger subdir set to: " << cwd_subdir << std::endl;
    return (*this);
  }
  Logger& setTimeElapsedMS(const long& x) {
    std::cout << "logger elapsed time (ms) set to: " << x << std::endl;
    return (*this);
  }
  Logger& setTimeElapsed(const std::string& x) {
    std::cout << "logger elapsed time set to: " << x << std::endl;
    return (*this);
  }
  Logger& setSuccess(bool suc) {
    std::cout << "logger success set to: " << suc << std::endl;
    return (*this);
  }
  Logger& setError(const std::string& what) {
    std::cout << "logger error set to: " << what << std::endl;
    return (*this);
  }
  Logger& setTarget(const std::string& target) {
    std::cout << "logger target set to: " << target << std::endl;
    return (*this);
  }
  Logger& setPlatform(const folly::Optional<std::string>& platform) {
    if(!platform.has_value())
      std::cout << "logger platform set to Nothing" << std::endl;
    else
      std::cout << "logger platform set to: " << platform.value() << std::endl;
    return (*this);
  }
  Logger& setFile(const std::string& file) {
    std::cout << "logger file set to: " << file << std::endl;
    return (*this);
  }
  Logger& setCompileError(bool err) {
    std::cout << "logger compile error set to: " << err << std::endl;
    return (*this);
  }
  Logger& setFactBufferSize(bool sz) {
    std::cout << "logger fact buffer size set to: " << sz << std::endl;
    return (*this);
  }
  Logger& setFactCacheSize(bool sz) {
    std::cout << "logger fact cache size set to: " << sz << std::endl;
    return (*this);
  }
};
#endif

struct SourceFile {
  std::string target;
  folly::Optional<std::string> platform;
  std::string dir;
  std::string file;
};

/**
 * A helper class to log indexing actions.
 *
 * Allows separating logging code that requires RTTI, from code inheriting
 * Clang classes which are normally built without RTTI.
 */
struct ActionLogger {
  using Clock = std::chrono::steady_clock;
  using FactStats = facebook::glean::cpp::FactStats;
  using CacheStats = facebook::glean::cpp::BatchBase::CacheStats;

  explicit ActionLogger(const std::string& name,
                        const std::string& task,
                        const std::string& request,
                        const std::string& repo_name,
                        const std::string& repo_hash,
                        const uint32_t worker_index,
                        const std::string& origin,
                        const std::string& cwd_subdir,
                        bool log = true);
  ~ActionLogger();

  bool log_index(const SourceFile &source,
                 const FactStats &buf_stats,
                 const CacheStats &cache_stats,
                 std::function<bool()> &&callback);
  void log(std::function<void()> &&callback);

private:
  bool enabled;
  Clock::time_point start;
  std::unique_ptr<Logger> logger;
};

}
}
}
