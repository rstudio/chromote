# with_chrome_version() works

    Code
      with_chrome_version("128.0.6612.0", with_retries(try_chromote_info))
    Output
      ---- {chromote} ----
         System: aarch64-apple-darwin20
      R version: R version 4.4.2 (2024-10-31)
       chromote: 0.4.0.9000
      
      ---- Chrome ----
         Path: ~/Library/Caches/org.R-project.R/R/chromote/chrome/128.0.6612.0/chrome-mac-arm64/Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing
      Version: Google Chrome for Testing 128.0.6612.0
         Args: --headless --force-color-profile=srgb --disable-extensions
               --mute-audio

