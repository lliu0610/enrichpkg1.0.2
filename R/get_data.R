# File: R/get_data.R

#' 获取包所需的大数据文件
#'
#' 这个函数会检查本地缓存中是否存在所需的数据文件。
#' 如果不存在，它会从指定的 URL 下载文件。
#' 这是包内部使用的函数，所以不导出 (@noRd)。
#'
#' @return 返回处理好的数据对象的 R 数据框/列表。
#' @importFrom rappdirs user_cache_dir
#' @importFrom utils download.file askYesNo
#' @noRd
.get_data <- function() {

  # 1. 定义 URL 和本地缓存路径
  # 定义URL
  data_url <- "https://github.com/lliu0610/enrichpkg/releases/download/v1.0.2-data/enrichpkg_data.rds"

  # 定义本包的缓存目录
  cache_dir <- rappdirs::user_cache_dir(appname = "enrichpkg")

  # 确保缓存目录存在
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # 定义最终的本地文件路径
  cache_file_path <- file.path(cache_dir, "enrichpkg_data.rds")

  # 2. 检查文件是否已缓存，如果否则下载
  if (!file.exists(cache_file_path)) {

    # 对于 CRAN，与用户交互非常重要
    if (interactive()) {
      message("The 'enrichpkg' package needs to download a data file (approx. 88.20 MB).")
      message("This will be cached locally and will only happen once.")
      permission <- utils::askYesNo("Do you want to download this file now?")
      if (!isTRUE(permission)) {
        stop("Data download cancelled by the user.", call. = FALSE)
      }
    }

    # 下载文件
    message("Downloading data to: ", cache_file_path)
    tryCatch({
      #options(download.file.method = "wininet")
      utils::download.file(url = data_url, destfile = cache_file_path, mode = "wb")
    }, error = function(e) {
      stop("Failed to download the data file. Please check your internet connection.", call. = FALSE)
    })
  }

  # 3. 从缓存中读取 RDS 文件并返回
  readRDS(cache_file_path)
}
