	source('blame.R');
	object <- read.corecomp();
	core.order <- NULL;

		# ghost plot for coordinates:
				n.core <- dim(object$info)[1L];
				y.max <- max(na.omit(object$info$length));
				y.seq <- as.integer(seq(0, y.max, by=y.max/(n.core-1)));
				
				plot(1L:n.core, y.seq, type='n', ann=FALSE, xaxt='n', bty='n', xlim=c(1L, n.core+1L), ylim=c(y.max, 0), ); # TODO: set device and fin dimensions based on (strwidth,strheight) for pdf output, and an option pdf=FALSE to control if plotting goes to grDevice or a file
				
				# a function to order cores by lat-lon:
				order.by.dist <- function(object) {
								# turn the value of dist() into a more manageable data.frame:
								dist(object)->d;
								times<-((dim(object)[[1L]]-1L):0L);
								a <- rep(rownames(object),times=times);
								b <- NULL;
								for (i in times[-1]) b <- c(b,rownames(object)[(length(rownames(object))-i):length(rownames(object))]);
								core.dist <- data.frame(a=a, b=b, d=as.vector(d));
								first.core <- as.character(core.dist$a[which(core.dist$d == max(core.dist$d))]);
								not.first.core <- core.dist[which(core.dist$a == first.core | core.dist$b == first.core),];
								not.first.core.ordered <- not.first.core[order(not.first.core$d),-3L];
								other.cores <- as.character(not.first.core.ordered[not.first.core.ordered != first.core]);
								cores.ordered <- c(first.core, other.cores);
								return(cores.ordered);
								}

				if (is.null(core.order)) {
								if (anyNA(object$info[c('lat','lon')])) {
												core.order <- rownames(object$info);
												}else{
																coords <- with(object$info, data.frame(x=lon, y=lat));
																rownames(coords) <- rownames(object$info);
																core.order <- order.by.dist(coords)
																}
								}
				# core plot variables:
				core.w = .3;		# fractional width for the core box
				event.w = .35;	# fractional width for event layers
				diag.w = .05;		# fractional space for diagonal conectors between event/age and labels
				label.w = 1 - (event.w + 2 * diag.w);
				padding = 1.5		# a factor for strheight, as labels padding
				
				# plot the cores:
				xleft <- 1L:n.core;
				ybottom <- object$info[core.order,'length'];
				xright <- xleft + core.w;
				ytop <- 0;
				rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop);
				mtext(core.order, at=1L:n.core, ); # TODO: a mechanism for ensuring core names will not collide
				mtext('Depth (cm)',side=2, line=2.5, )

				# plot events:
				tephra <- object$event[object$event$type == 'tephra',];
				xleft <- match(tephra$core, core.order);
				ybottom <- tephra$from;
				xright <- xleft + event.w;
				ytop <- tephra$to;
				rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, border=NA, col='black');

				# plot ages:
				known.age <- na.omit(object$age);
				age <- list(x=match(known.age$core, core.order) + event.w, y=known.age$depth);
				points(age, pch='*');
				
				# plot labels:
				known.tephra <- na.omit(tephra[tephra$event_id != '', ]);
				tephra.names <- unique(as.character(known.tephra$event_id));
				shared.tephra <- NULL;
				for(i in tephra.names){
								shared.tephra <- c(shared.tephra, core.order %in% as.character(known.tephra$core[known.tephra$event_id == i]))
								};
				shared.tephra <- matrix(shared.tephra, nrow=length(tephra.names), ncol=length(core.order), dimnames=list(event=tephra.names, core=core.order), byrow=TRUE);
				labels.at <- data.frame(x=append(age$x + diag.w, match(known.tephra$core, core.order) + event.w + diag.w),
								y=append(age$y, with(known.tephra, (from + to) / 2)),
								labels=append(paste(known.age$age, known.age$error, sep='\u00b1'), as.character(known.tephra$event_id)),
								type=rep(c('age','tephra'), times=c(length(age$x),length(known.tephra$event_id))),
								core=append(as.character(known.age$core), as.character(known.tephra$core)),
								stringsAsFactors=FALSE);
				labels.at <- labels.at[order(labels.at$x, labels.at$y),];
				labels.at$top <- with(labels.at, y - (strheight(labels) * padding) / 2);
				labels.at$bottom <- with(labels.at, y + (strheight(labels) * padding) / 2);
				push = NULL;
				for (i in 2L:dim(labels.at)[1L]){
								if (labels.at$core[i] == labels.at$core[i - 1L]){
												if (labels.at$top[i] <= labels.at$bottom[i - 1L]){
																push <- labels.at[i - 1L, 'bottom'] + strheight(labels.at$labels[i]) * (padding - 1) - labels.at[i, 'top'];
																labels.at[i, 'top'] <- labels.at$top[i] + push;
																labels.at[i, 'bottom'] <- labels.at$bottom[i] + push;
																labels.at[i, 'y'] <- labels.at$y[i] + push;
																}
												}
								} # TODO: labels should move upwards if possible for a balanced output, currently just pushing downwards
				
				# first the tephra bands:
				band <- list(x=NULL, y=NULL);
				x0 <- NULL;
				for (i in rownames(shared.tephra)){
								for (j in colnames(shared.tephra)){
												if (shared.tephra[i,j]){
																x0 <- match(j, core.order);
																band$x <- c(x0  + 1 - diag.w, x0 + event.w + diag.w, rep(x0 + event.w, times = 2), x0 + event.w + diag.w, x0 + 1 - diag.w);
																band$y <- c(rep(labels.at[labels.at$core == j & labels.at$labels == i, 'top'], times = 2),
																			known.tephra[known.tephra$core == j & known.tephra$event_id == i, 'to'],
																			known.tephra[known.tephra$core == j & known.tephra$event_id == i, 'from'],
																			rep(labels.at[labels.at$core == j & labels.at$labels == i, 'bottom'], times = 2));
																
																if (match(j, colnames(shared.tephra)) < length(colnames(shared.tephra))){
																				if (shared.tephra[i, match(j, colnames(shared.tephra)) + 1]){
																								band$x <- append(band$x, rep(x0 + 1, times = 2));
																								band$y <- append(band$y,
																											c(known.tephra[known.tephra$core == colnames(shared.tephra)[match(j, colnames(shared.tephra)) + 1L] & known.tephra$event_id == i, 'from'],
																											known.tephra[known.tephra$core == colnames(shared.tephra)[match(j, colnames(shared.tephra)) + 1L] & known.tephra$event_id == i, 'to']));
																								};
																				};
																polygon(band, col='black', border=NA);
																};
												};
								};
				
				# now tephra labels in white:
				text(x=labels.at[labels.at$type == 'tephra', 'x'], y=labels.at[labels.at$type == 'tephra', 'y'], labels=labels.at[labels.at$type == 'tephra', 'labels'], col = 'white', pos = 4);

				# finaly age labels:
				age <- data.frame(age);
				age <- age[order(age$x, age$y), ];
				segments(x0=age$x, y0=age$y, x1=labels.at[labels.at$type == 'age', 'x'], y1=labels.at[labels.at$type == 'age', 'y'], lty = 'dotted');
				text(x=labels.at[labels.at$type == 'age', 'x'], y=labels.at[labels.at$type == 'age', 'y'], labels=labels.at[labels.at$type == 'age', 'labels'], pos = 4);

