import React from 'react';
import useThemeContext from '@theme/hooks/useThemeContext'; //docs: https://v2.docusaurus.io/docs/2.0.0-alpha.69/theme-classic#usethemecontext

const ImageSwitcher = ({lightImageSrc, darkImageSrc, imageHeight}) => {
  const { isDarkTheme } = useThemeContext();

  return (
    <img src={isDarkTheme ? darkImageSrc : lightImageSrc} height={imageHeight} />
  )
}

export default ImageSwitcher;