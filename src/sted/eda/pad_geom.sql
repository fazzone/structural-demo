with pad_to_geom as (
select
  case shape
    when 'rect' then buildmbr(0,0, width_mm,height_mm)
    when 'circle' then makecircle(0,0,width_mm/2)
    else NULL
  end as padgeom
  , * 
from kicad_pad
)

select d_mm as svg1, assvg(padgeom) as svg2, * from pad_to_geom






with pad_to_geom as (
select
  case shape
    when 'rect' then buildmbr(-width_mm/2, -height_mm/2, width_mm/2, height_mm/2)
    when 'circle' then makecircle(0,0,width_mm/2)
    when 'roundrect' then
      st_convexhull(
        collect(
          collect(makearc( width_mm/2,  height_mm/2, 0.2,    0,  90),
                  makearc( width_mm/2, -height_mm/2, 0.2,  -90,   0)),
          collect(makearc(-width_mm/2,  height_mm/2, 0.2,   90, 180),
                  makearc(-width_mm/2, -height_mm/2, 0.2, -180, -90))))
    else NULL end as padgeom
  , * 
from kicad_pad
)
select type,shape, d_mm as svg1, assvg(padgeom) as svg2, d_mm as d1, '-1 -1 2 2' as viewbox2
--, assvg(padgeom) as d2
, * from pad_to_geom
where shape = 'roundrect'
