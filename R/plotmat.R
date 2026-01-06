##modifying plotmat from LBSPR package to be able to customize ggplot aesthetics

plotmat_targets <- list(
  #' Plot the maturity-at-length and selectivity-at-length curves
  #'
  #' A function that plots the maturity-at-length and selectivity-at-length curves
  #'
  #' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
  #' @param size.axtex size of the axis text
  #' @param size.title size of axis title
  #' @param size.leg size of legend text
  #' @param useSmooth use the smoothed estimates?
  #' @param Title optional character string for plot title
  #' @return a ggplot object
  #' @author A. Hordyk
  #'
  #' @importFrom grDevices colorRampPalette
  #' @importFrom tidyr gather
  #' @importFrom RColorBrewer brewer.pal
  #' @export
  tar_target(
    plotMat,
    function(
      LB_obj = NULL,
      size.axtex = 12,
      size.title = 14,
      size.leg = 12,
      useSmooth = TRUE,
      Title = NULL
    ) {
      if (class(LB_obj) != "LB_obj" & class(LB_obj) != "LB_pars") {
        stop(
          "LB_obj must be of class 'LB_obj' or class 'LB_pars'",
          call. = FALSE
        )
      }

      Lens <- seq(from = 0, to = 150, by = 1)
      # Length at Maturity
      if (length(LB_obj@L_units) > 0) {
        XLab <- paste0("Total Length (", LB_obj@L_units, ")")
      } else {
        XLab <- "Total Length"
      }
      LenMat <- 1.0 /
        (1 + exp(-log(19) * (Lens - LB_obj@L50) / (LB_obj@L95 - LB_obj@L50)))
      DF <- data.frame(Lens = Lens, Dat = LenMat, Line = "Maturity")
      Dat <- Proportion <- Line <- SelDat <- Year <- NULL # hack to get past CRAN check

      if (class(LB_obj) == "LB_obj") {
        years <- LB_obj@Years
        if (length(years) < 1) {
          years <- 1
        }

        if (useSmooth & length(LB_obj@Ests) > 0) {
          SL50 <- LB_obj@Ests[, "SL50"]
          SL95 <- LB_obj@Ests[, "SL95"]
        }
        if (!useSmooth | length(LB_obj@Ests) == 0) {
          SL50 <- LB_obj@SL50
          SL95 <- LB_obj@SL95
        }

        spread_colors <- function(n) {
          colorlist <- c(
            "#332288",
            "#88ccee",
            "#44aa99",
            "#117733",
            "#999933",
            "#ddcc77",
            "#cc6677",
            "#aa4499",
            "#882255"
          )

          if (n > length(colorlist) || n < 1) {
            stop("can only take up to 9 years")
          }

          indices <- round(seq(1, length(colorlist), length.out = n))
          indices[1] <- 1
          indices[length(indices)] <- length(colorlist)

          return(colorlist[indices])
        }

        cols <- spread_colors(length(years))

        LenSel <- sapply(1:length(years), function(X) {
          1.0 /
            (1 + exp(-log(19) * (Lens - (SL50[X])) / ((SL95[X]) - (SL50[X]))))
        })
        LenSel <- data.frame(LenSel, check.names = FALSE)
        colnames(LenSel) <- years
        longSel <- gather(LenSel, "Year", "SelDat")
        longSel$Lens <- DF$Lens
        mplot <- ggplot(DF, aes(x = Lens, y = Dat)) +
          geom_line(
            aes(x = Lens, y = SelDat, color = Year),
            longSel,
            linewidth = 1
          ) +
          geom_line(aes(color = "Maturity"), linewidth = 1.5) +
          guides(color = guide_legend(title = "Est. Selectivity")) +
          scale_color_manual(values = c(cols, "black")) +
          xlab(XLab) +
          ylab("Proportion") +
          xlim(0, max(DF$Lens)) +
          theme_bw() +
          theme(
            axis.text = element_text(size = size.axtex),
            axis.title = element_text(size = size.title, face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "inside",
            legend.position.inside = c(0.8, (0.2 + 0.025 * length(years))),
            plot.title = element_text(lineheight = .8, face = "bold"),
            legend.text = element_text(size = size.leg),
            legend.title = element_text(size = size.leg)
          )
      }
      if (!(is.null(Title)) & class(Title) == "character") {
        mplot <- mplot + ggtitle(Title)
      }
      mplot
    }
  )
)
