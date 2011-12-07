
/* ************************************************************************

************************************************************************ */

/**
 * The main tool bar widget
 */
qx.Class.define("bsk.view.ChartController",
{
    extend : qx.ui.container.Composite,

    construct : function(biz, Data, chartModel) {
        this.chartModel = chartModel;
        this.initData = Data;
        this.biz = biz;
        this.base(arguments, new qx.ui.layout.VBox());

        this.toolbar = new bsk.view.ToolBar(this.biz);
        this.add(this.toolbar);
        this.addListenerOnce("appear", this._onAppears, this);


        var dCont = new qx.ui.container.Composite(new qx.ui.layout.Dock());
        var w1 = new qx.ui.core.Widget();
        var w2 = new qx.ui.core.Widget();
        var w3 = new qx.ui.core.Widget();
        var w4 = new qx.ui.core.Widget();
        dCont.add(w1, {edge:"north", flex:0})
        dCont.add(w2, {edge:"west", flex:0})
        dCont.add(w3, {edge:"south", flex:0});
        dCont.add(w4, {edge:"east", flex:0});

        this.add(dCont,  {flex : 1});

        this.scrollCnt = new qx.ui.container.Scroll();
        var vbox_layout  = new qx.ui.layout.VBox();
        vbox_layout.setSpacing(100);
        this.chartCnt = new qx.ui.container.Composite(vbox_layout);
        //this.chartCnt.setHeight(this.chartModel.Y.length*500);
        this.scrollCnt.add(this.chartCnt);
      
      
        this.childChartCnt = Array();
        for(var j=0;j<=this.chartModel.Y.length-1;j++){
            this.childChartCnt[j] = new qx.ui.container.Composite(new qx.ui.layout.VBox());
            this.childChartCnt[j].setHeight(500);
            this.chartCnt.add(this.childChartCnt[j]);
        }
        
     
dCont.add(this.scrollCnt, {edge : "center", flex : 1});
    },

    members : {
        _onAppears : function() {
            
            var rnds = {'line': jQuery.jqplot.LineRenderer, 'bar': jQuery.jqplot.BarRenderer, 'pie': jQuery.jqplot.PieRenderer};
            for(var j=0;j<=this.chartModel.Y.length-1;j++){
            var row_name=this.chartModel.Y[j].name;
            var row_alias=this.chartModel.Y[j].alias;
            var chart_type = 'line';
            if (this.chartModel.Y[j].chart_type) chart_type=this.chartModel.Y[j].chart_type;
            var x_angle = 0;
            if (this.chartModel.Y[j].x_angle) x_angle=this.chartModel.Y[j].x_angle;
            if (this.chartModel.Y[j].x_label) var x_label=this.chartModel.Y[j].x_label; else var x_label=null;
            if (this.chartModel.Y[j].y_label) var y_label=this.chartModel.Y[j].y_label; else var y_label=null;
            var dom = this.childChartCnt[j].getContainerElement().getDomElement();
            qx.bom.element.Attribute.set(dom, 'id', 'jqxPlotBox'+j);
            var lines = [];
            var watchesLine = []
            var myGraph;
            var keys = new Array();
            for(var key in this.initData) {
                keys.push(key);
            }
            keys.sort();
            
            if (this.chartModel.Y[j].color) var series_colors = [this.chartModel.Y[j].color];
            else var series_colors = undefined;
            var series_renderers = [ {label:row_alias, renderer: rnds[chart_type]} ];
            if (this.chartModel.Y[j].stacked!=undefined){
                for (var v=0;v<this.chartModel.Y[j].stacked.length;v++){
                    series_colors[v] = this.chartModel.Y[j].stacked[v].color;
                    series_renderers[v] = {label:row_alias, renderer: rnds[chart_type]};
                }
            }
            
            for(var m=0;m<keys.length;m++) {
                var key = keys[m];
                if (this.chartModel.Y[j].stacked!=undefined){
                    for (v=0;v<this.chartModel.Y[j].stacked.length;v++){
                        var local_row_name = this.chartModel.Y[j].stacked[v].name;
                        if (watchesLine[v]==undefined) watchesLine[v]=[];
                        watchesLine[v].push([this.initData[key][this.chartModel.X], this.initData[key][local_row_name]]);
                    }
                }
                else watchesLine.push([this.initData[key][this.chartModel.X], this.initData[key][row_name]]);
            }
            if (this.chartModel.Y[j].stacked==undefined) watchesLine = [watchesLine];
            myGraph = jQuery.jqplot('jqxPlotBox'+j, watchesLine, {
                stackSeries: (this.chartModel.Y[j].stacked!=undefined),
                legend:{show:false, location:'ne'}, 
                series: series_renderers, 
                title:row_alias,
                axesDefaults : {
                    tickRenderer: jQuery.jqplot.CanvasAxisTickRenderer,
                    min: 0
                    },
                axes:{
                    xaxis:{
                        renderer : jQuery.jqplot.CategoryAxisRenderer,
                        label: x_label,
                        showLabel: (x_label!=null),
                        tickOptions: {
                            angle: x_angle,
                            fontSize: '10pt'
                        }
                    },
                    yaxis:{
                        label: y_label,
                        showLabel: (y_label!=null),
                        tickOptions: { formatString: this.chartModel.Y[j].type, formatter: function(format_string, value){ if (format_string=='sec') { return Math.floor(value/3600).toString()+":"+Math.floor((value-Math.floor(value/3600)*3600)/60).toString()+":"+Math.floor(value % 60).toString(); } else return Math.floor(value).toString(); } }
                        }
                }
                ,seriesColors: series_colors
                //,series:[{lineWidth:4, markerOptions:{style:'square'}}]
            });
            myGraph.redraw();
            
            }

/*
            var data = [ [500, 350], [200, 300], [800, 800], [900, 650], [400, 100] ];
            var labels = [ {label: 'frogs'}, {label: 'buzzards'}, {label: 'cows'}, {label: 'deer'}, {label: 'mice'}];
            var xLabels = ['2005', '2009'];

            var myGraph = jQuery.jqplot('jqxPlotBox', data, {
                title: {text: title, fontSize: '12pt'},
                legend: {show: true, location: 'ne', fontSize: '8pt'},
                seriesDefaults: {
                    renderer: jQuery.jqplot.BarRenderer,
                    rendererOptions: {barPadding: 8, barMargin: 20}
                },
                series: labels,
                axes: {
                    xaxis: {
                        tickOptions: {
                            enableFontSupport: true,
                            fontFamily: 'Georgia',
                            fontSize: '10pt'
                        },
                        renderer: jQuery.jqplot.CategoryAxisRenderer,
                        ticks: xLabels,
                        enableFontSupport: true,
                        fontSize: '10pt'
                    },
                    yaxis: {
                        min: 0, max: 1100, numberTicks: 12,
                        tickOptions: {
                            enableFontSupport: true,
                            fontFamily: 'Georgia',
                            fontSize: '10pt'
                        },
                        labelRenderer: jQuery.jqplot.CanvasAxisLabelRenderer,
                        labelOptions: {
                            enableFontSupport: true,
                            fontFamily: 'Georgia',
                            fontSize: '12pt'
                        },
                        label: 'y-axis',
                        fontSize: '10pt'
                    }
                },
                cursor: {
                    zoom: true, clickReset: true,
                    showTooltip: true
                }
            });
*/
        }

    }

});

