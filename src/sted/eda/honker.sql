with recursive padshapes as (
select
  NULL as use
  , NULL as text
  , printf('kcp%d', kp.id) as def
  , NULL as transform
  , printf('pad %s', kp.type) as class
  , case when shape = 'oval' then width_mm
         else NULL end as stroke_width
  , d_mm as d
from kicad_pad kp
) , pads as (
select
  printf('kcp%d', fpp.kicad_pad_id) as use
  , NULL as text
  , printf('kcf%d', fp.id) as def
  , printf('translate(%f,%f)', x_mm, y_mm) as transform
  , NULL as class
  , NULL as stroke_width
  , NULL as d
from kicad_footprint fp
left join kicad_footprint_pad fpp on fpp.kicad_footprint_id = fp.id
) , draws as (
select
  NULL as use
  , d.text as text
  , printf('kcf%d', fp.id) as def
  , case when text is not null then printf('translate(%f,%f)', text_x_mm, text_y_mm)
    end as transform
  , replace(d.layer, '.', ' ') as class
  , width_mm as stroke_width
  , group_concat(d_mm, ' ') as d
from kicad_footprint fp
left join kicad_footprint_draw d on fp.id = d.kicad_footprint_id
group by d.text, d.layer, width_mm, fp.id
), u as (
  select * from padshapes
  union all
  select * from pads
  union all
  select * from draws
),
deps(def, use) as (
  select u.def, u.use from u where u.def = (select printf('kcf%d', id) from kicad_footprint where name = 'QFN-64-1EP_9x9mm_P0.5mm_EP7.3x7.3mm')
  union
  select u.def, u.use from u, deps where u.def = deps.use
)
select * from u where u.def in (select def from deps)
