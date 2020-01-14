`cfaGaps` <-
function(
	gap_data,
	gap_groups=NULL,
	gap_plot_title="Non-Parametric Gap Plot",
	gap_plot_fig_width=13,
	gap_plot_fig_height=8.5,
	gap_plot_file_path="Visualizations/Gaps/",
	gap_plot_file_name="Gaps",
	gap_plot_output_format="PDF") {

	if ("PNG" %in% gap_plot_output_format) {

		print("PNG Plots Coming Soon!")

	}


	#####################################################################
	###
	### PDF output
	###
	#####################################################################

	if ("PDF" %in% gap_plot_output_format) {

		# Create directory for file

		if (!is.null(gap_plot_file_path)) {
		    dir.create(gap_plot_file_path, recursive=TRUE, showWarnings=FALSE)
			file.path.and.name <- file.path(gap_plot_file_path, gap_plot_file_name)
		} else {
			file.path.and.name <- gap_plot_file_name
		}

		# Create viewports

		figure.vp <- viewport(layout = grid.layout(3, 3, widths = unit(c(0.8, 9.5, 2.7)*gap_plot_fig_width/13, rep("inches", 3)),
							  heights = unit(c(0.2, 7.6, 0.8)*gap_plot_fig_height/8.5, rep("inches", 3))),
							  gp=gpar(cex=gap_plot_fig_width/13))

	  	title.vp <- viewport(name="title.vp",
		                      layout.pos.row=1, layout.pos.col=1:3,
		                      xscale=c(0,1),
		                      yscale=c(0,1),
		                      gp=gpar(fill="transparent"))

		right.legend.vp <- viewport(name="right.top.legend.vp",
				   			  layout.pos.row=2, layout.pos.col=3,
							  xscale=c(0,1),
							  yscale=c(0,1),
							  gp=gpar(fill="transparent"))

	    vaxis.vp <- viewport(name="vaxis.vp",
	                          layout.pos.row=2, layout.pos.col=1,
#	                          xscale=c(0,1),
#	                          yscale=extendrange(bubble_plot_configs.BUBBLE_Y_TICKS, f=0.025),
	                          gp=gpar(fill="transparent", cex=1.2))

 	    chart.vp <- viewport(name="chart.vp",
  		                  	  layout.pos.row=2, layout.pos.col=2,
#  		                  	  xscale=extendrange(bubble_plot_configs.BUBBLE_X_TICKS, f=0.025),
#  		                  	  yscale=extendrange(bubble_plot_configs.BUBBLE_Y_TICKS, f=0.025),
  		                  	  gp=gpar(fill="transparent"))

  		haxis.vp <- viewport(name="haxis.vp",
  		                      layout.pos.row=3, layout.pos.col=2,
# 		                      xscale=extendrange(bubble_plot_configs.BUBBLE_X_TICKS, f=0.025),
#		                      yscale=c(0,1),
  		                      gp=gpar(fill="transparent", cex=1.2))


		pdf(file=file.path.and.name, width=gap_plot_fig_width, height=gap_plot_fig_height)


		# Push figure.vp

		pushViewport(figure.vp)


		# Push chart.vp

		pushViewport(chart.vp)

		popViewport() ## Pop chart.vp


		popViewport() ## Pop figure.vp


		dev.off() ### Turn off PDF device



	} ### END PDF gap_plot_output_format

	if ("SVG" %in% gap_plot_output_format) {

		print("SVG Plots Coming Soon!")

	}

	if ("PLOTLY" %in% gap_plot_output_format) {

		print("Coming Soon!")

	}

} ### END cfaGaps
