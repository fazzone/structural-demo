with recursive deps(def, use) as (
  select def, use from json_each(?) je, svg_atlas where def = je.value
  union
  select a.def, a.use from svg_atlas a, deps where a.def = deps.use
)
--select * from svg_atlas a where a.def in (select def from deps)
select def from deps
