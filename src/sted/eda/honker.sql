
-- insert into svg_atlas select * from svg_atlas_view

--select 'kcf1' as svg1, 'kcf2' as svg2, 'kcf3' as svg3
select printf('kcf%s', id) as svg, * from kicad_footprint

