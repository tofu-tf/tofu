---
id: optics
title: Tofu optics
---

## Installation
`"ru.tinkoff" %% "tofu" % Versions.tofu`  
or as standalone dependencies:   
`"ru.tinkoff" %% "tofu-optics-core" % Versions.tofu`  
`"ru.tinkoff" %% "tofu-optics-macro" % Versions.tofu`  
`"ru.tinkoff" %% "tofu-optics-interop" % Versions.tofu`  
 
## Functionality
Tofu contains its own optics library with some notable features:
- subtyping: You don't need to convert more powerful optic to weaker one
- unified composition operator

Hierarchy
---------

<img src="/tofu/docs_img/optics-hierarchy.png" height="700">

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
