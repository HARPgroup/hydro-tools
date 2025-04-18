% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RomPropertyTree.R
\name{RomPropertyTree}
\alias{RomPropertyTree}
\title{Nested Property Object}
\value{
Entity class of type RomPropertyTree
}
\description{
Object for storing a single feature with attribute and timeseries related
}
\details{
Has standard methods for managing data and meta data
}
\examples{
NA
}
\seealso{
NA
}
\section{Super classes}{
\code{\link[hydrotools:RomEntity]{hydrotools::RomEntity}} -> \code{\link[hydrotools:RomDataGroup]{hydrotools::RomDataGroup}} -> \code{RomPropertyTree}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{base_entity_type}}{kind of entity}

\item{\code{pk_name}}{the name of this entity's pk column}

\item{\code{prop_list}}{The raw property rows returned}

\item{\code{mps}}{linked features}

\item{\code{geom}}{feature geometry WKT}

\item{\code{sql_select_from}}{Enhanced form of query overrides default SQL guess.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RomPropertyTree-new}{\code{RomPropertyTree$new()}}
\item \href{#method-RomPropertyTree-get_id}{\code{RomPropertyTree$get_id()}}
\item \href{#method-RomPropertyTree-to_list}{\code{RomPropertyTree$to_list()}}
\item \href{#method-RomPropertyTree-from_list}{\code{RomPropertyTree$from_list()}}
\item \href{#method-RomPropertyTree-save}{\code{RomPropertyTree$save()}}
\item \href{#method-RomPropertyTree-load_data}{\code{RomPropertyTree$load_data()}}
\item \href{#method-RomPropertyTree-clone}{\code{RomPropertyTree$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="delete"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-delete'><code>hydrotools::RomEntity$delete()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="get_prop"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-get_prop'><code>hydrotools::RomEntity$get_prop()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="get_vardef"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-get_vardef'><code>hydrotools::RomEntity$get_vardef()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="handle_config"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-handle_config'><code>hydrotools::RomEntity$handle_config()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="insure_varid"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-insure_varid'><code>hydrotools::RomEntity$insure_varid()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="load_plugin"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-load_plugin'><code>hydrotools::RomEntity$load_plugin()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="propvalues"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-propvalues'><code>hydrotools::RomEntity$propvalues()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="save_field"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-save_field'><code>hydrotools::RomEntity$save_field()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="set_prop"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-set_prop'><code>hydrotools::RomEntity$set_prop()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="RomEntity" data-id="tsvalues"><a href='../../hydrotools/html/RomEntity.html#method-RomEntity-tsvalues'><code>hydrotools::RomEntity$tsvalues()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RomPropertyTree-new"></a>}}
\if{latex}{\out{\hypertarget{method-RomPropertyTree-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RomPropertyTree$new(datasource = NULL, config, load_remote = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{datasource}}{RESTful repository object}

\item{\code{config}}{list of attributes to set, see also: to_list() for format}

\item{\code{load_remote}}{automatically query REST dataa source for matches?}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
object instance
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RomPropertyTree-get_id"></a>}}
\if{latex}{\out{\hypertarget{method-RomPropertyTree-get_id}{}}}
\subsection{Method \code{get_id()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RomPropertyTree$get_id()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
get_id the unique id of this entity alias to remote pkid, subclassed as function
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RomPropertyTree-to_list"></a>}}
\if{latex}{\out{\hypertarget{method-RomPropertyTree-to_list}{}}}
\subsection{Method \code{to_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RomPropertyTree$to_list()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
list of object attributes suitable for input to new() and from_list() methods
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RomPropertyTree-from_list"></a>}}
\if{latex}{\out{\hypertarget{method-RomPropertyTree-from_list}{}}}
\subsection{Method \code{from_list()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RomPropertyTree$from_list(config)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{config}}{list of attributes to set, see also: to_list() for format}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RomPropertyTree-save"></a>}}
\if{latex}{\out{\hypertarget{method-RomPropertyTree-save}{}}}
\subsection{Method \code{save()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RomPropertyTree$save(push_remote = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{push_remote}}{update locally only or push to remote database}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RomPropertyTree-load_data"></a>}}
\if{latex}{\out{\hypertarget{method-RomPropertyTree-load_data}{}}}
\subsection{Method \code{load_data()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RomPropertyTree$load_data(config, load_remote)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{config}}{input attributes}

\item{\code{load_remote}}{automatically query remote data source for matches?}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
the data from the remote connection
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RomPropertyTree-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RomPropertyTree-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RomPropertyTree$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
