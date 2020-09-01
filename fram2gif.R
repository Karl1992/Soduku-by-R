frames2gif <- function(pathIn='',
                       pathOut='',
                       ImageMagick_path='',
                       resize_ratio=1,
                       delay=40,
                       frameFormat='png',
                       everyFrame=1){
  
    tempdir <- paste0(pathIn, '/temp')
  dir.create(tempdir)
  
  files <- list.files(pathIn, pattern=paste0('*.', frameFormat), recursive=FALSE, full.names=TRUE)
  index <- seq(1, length(files), by=everyFrame)
  file.copy(files[index], tempdir)
  
  command <- paste(ImageMagick_path,
                   '-resize', paste0(as.integer(100L*resize_ratio), '%'),
                   '-delay', delay, '-loop 1', 
                   paste0(tempdir,'/*.', frameFormat),
                   pathOut)
  #system('F:R_tutorials/gif/ImageMagick-7.0.8-64bit/convert -resize 90% -delay 40 *.png result.gif')
  system(command)
  
  ## delete temp dir
  unlink(tempdir, recursive=TRUE, force=TRUE)
}

frames2gif(pathIn = "./", pathOut = "testsudoku.gif", ImageMagick_path= "D:/Games/ImageMagick-7.0.8-Q16/convert")
