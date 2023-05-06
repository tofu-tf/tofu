---
id: optics
title: Tofu optics
---

## Installation
`"tf.tofu" %% "tofu" % tofu-version`  
or as standalone dependencies:   
`"tf.tofu" %% "tofu-optics-core" % tofu-version`  
`"tf.tofu" %% "tofu-optics-macro" % tofu-version`  
`"tf.tofu" %% "tofu-optics-interop" % tofu-version`  
 
## Functionality
Tofu contains its own optics library with some notable features:
- subtyping: You don't need to convert more powerful optic to weaker one
- unified composition operator

Hierarchy
---------

import ThemedImage from '@theme/ThemedImage';

<ThemedImage
  alt="Docusaurus themed image"
  sources={{
    light: '/docs_img/optics-hierarchy.svg',
    dark: '/docs_img/optics-hierarchy-dark.svg',
  }}
/>


Intuition
---------

If you ever used [lens](https://github.com/ekmett/lens) optics' names will be quite familiar to you,
but for a [Monocle](https://github.com/julien-truffaut/Monocle) user `Tofu`'s naming may be confusing. The following table may clear things up:

| Monocle | tofu |
|---------|------|
| Iso | Equivalent |
| Prism | Subset |
| Lens | Contains |
| Optional | Property |
| Traversal | Items |
| Getter | Extract |
| Fold | Folded |
| Setter | Update |
