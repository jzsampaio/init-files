(require 'ox-taskjuggler)
(setq org-taskjuggler-default-reports '("textreport report \"Plan\" {
formats html
header '== %title =='
center -8<-
[#Plan Plan] | [#Resource_Allocation Resource Allocation]
----
=== Plan ===
<[report id=\"plan\"]>
----
=== Resource Allocation ===
<[report id=\"resourceGraph\"]>
->8-
}
# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
headline \"Project Plan\"
columns bsi,
        name,
        start,
        end,
        effort,
        effortdone,
        effortleft,
        chart { width 1000 scale day }
loadunit days
hideresource 1
}
# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
headline \"Resource Allocation Graph\"
columns no, name, effort, chart { width 1000 scale day }
loadunit days
hidetask ~(isleaf() & isleaf_())
sorttasks plan.start.up
}"))
