---
id: optics
title: Tofu optics
---

Tofu contains its own optics library with some notable features:
- subtyping: You don't need to convert more powerful optic to weaker one
- unified composition operator

Hierarchy
---------

[![Optics Hierarchy](./optics-hierarchy.png)]

Intuition
---------

If You ever used [lens](https://github.com/ekmett/lens) optics' names will be quite familiar to You,
but for a [Monocle](https://github.com/julien-truffaut/Monocle) user Tofu's naming may be confusing. The following table may clear things up:

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
