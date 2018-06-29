# first draft on the blame software
# starting with loading data and creating a suitable object:

# data should be in a folder named 'input_files', containing four files:
# core_ages, core_event, core_hiatus, core_info, all with .csv extension.
# files should use '\t' as field separator and contain headers (this should change)

# file.path() should ensure platform independent paths...

read.corecomp <- function() {
				filein <- function(file_name, ...) read.csv(file.path(paste('input_files/core_', file_name, '.csv', sep='')), sep='\t', ...);
				corecomp <- list(ages=filein('ages'), event=filein('event'), hiatus=filein('hiatus'), info=filein('info', row.names='core', colClass=c(NA, NA, NA, NA, NA, NA, 'character')));
				class(corecomp)<-'corecomp';

				# check and fix:

				if (!all(levels(corecomp$age$core) %in% rownames(corecomp$info))) {
								warning(paste('Reading files: inconsistent data between core_ages and core_info, please check'));
								}

				if (!all(levels(corecomp$event$core) %in% rownames(corecomp$info))) {
								message(paste('Reading files: inconsistent data between core_event and core_info, please check'));
								}

				if (anyNA(corecomp$info$length)) {
								missing <- rownames(corecomp$info)[is.na(corecomp$info$length)];
								max.event.depth <- tapply(na.omit(corecomp$event$from), INDEX=corecomp$event$core[!is.na(corecomp$event$from)], FUN=max);
								max.age.depth <- tapply(na.omit(corecomp$age$depth), INDEX=corecomp$age$core[!is.na(corecomp$age$depth)], FUN=max);
								corecomp$info[missing,]$length <- pmax(max.event.depth[missing], max.age.depth[missing], na.rm=TRUE);
								message(cat('\n','IMPORTANT: the following cores had no "length" value in core_info:\n\n',missing,'\n\n','missing values were replaced by maximum depth within ages or events\n'));
								attributes(corecomp$info)$faked.lengths <- missing;
								}

				return(corecomp);
				}

summary.corecomp <- function(object) {
				n.core <- dim(object$info)[1L];
				n.basin <- length(levels(object$info$basin));
				cat('\n A compilation of', n.core, 'cores from', n.basin, 'basins:\n\n');
				object$info;
# keep writing this later...
				}

plot.corecomp <- function(object, core.order=NULL, rev=FALSE, ...) {
				# ghost plot for coordinates:
				n.core <- dim(object$info)[1L];
				y.max <- max(na.omit(object$info$length));
				y.seq <- as.integer(seq(0, y.max, by=y.max/(n.core-1)));
				
				plot(1L:n.core, y.seq, type='n', ann=FALSE, xaxt='n', bty='n', xlim=c(1L, n.core+1L), ylim=c(y.max, 0), ...); # TODO: set device and fin dimensions based on (strwidth,strheight) for pdf output, and an option pdf=FALSE to control if plotting goes to grDevice or a file
				
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
				diag.w = .1;		# fractional space for diagonal conectors between event/age and labels
				label.w = 1 - (event.w + 2 * diag.w);
				padding = 1.8		# a factor for strheight, as labels padding
				
				# plot the cores:
				xleft <- 1L:n.core;
				ybottom <- object$info[core.order,'length'];
				xright <- xleft + core.w;
				ytop <- 0;
				rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop);
				mtext(core.order, at=1L:n.core, ...); # TODO: a mechanism for ensuring core names will not collide
				mtext('Depth (cm)',side=2, line=2.5, ...)

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
				labels.at$top <- with(labels.at, y + (strheight(labels) * padding) / 2);	# strheight returns a negative number since y axis is inverted
				labels.at$bottom <- with(labels.at, y - (strheight(labels) * padding) / 2);
				push = NULL;
				for (i in 2L:dim(labels.at)[1L]){
								if (labels.at$core[i] == labels.at$core[i - 1L]){
												if (labels.at$top[i] <= labels.at$bottom[i - 1L]){
																push <- labels.at[i - 1L, 'bottom'] - strheight(labels.at$labels[i]) * (padding - 1) - labels.at[i, 'top'];
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
																			known.tephra[known.tephra$core == j & known.tephra$event_id == i, 'from'],
																			known.tephra[known.tephra$core == j & known.tephra$event_id == i, 'to'],
																			rep(labels.at[labels.at$core == j & labels.at$labels == i, 'bottom'], times = 2));
																
																if (match(j, colnames(shared.tephra)) < length(colnames(shared.tephra))){
																				if (shared.tephra[i, match(j, colnames(shared.tephra)) + 1]){
																								band$x <- append(band$x, rep(x0 + 1, times = 2));
																								band$y <- append(band$y,
																											c(known.tephra[known.tephra$core == colnames(shared.tephra)[match(j, colnames(shared.tephra)) + 1L] & known.tephra$event_id == i, 'to'],
																											known.tephra[known.tephra$core == colnames(shared.tephra)[match(j, colnames(shared.tephra)) + 1L] & known.tephra$event_id == i, 'from']));
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
				segments(x0=age$x, y0=age$y, x1=labels.at[labels.at$type == 'age', 'x'], y1=labels.at[labels.at$type == 'age', 'y'], lty = 'solid');
				text(x=labels.at[labels.at$type == 'age', 'x'], y=labels.at[labels.at$type == 'age', 'y'], labels=labels.at[labels.at$type == 'age', 'labels'], pos = 4);
							}
