% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dHVariablePluginDefault.R
\name{dHOMDataMatrix}
\alias{dHOMDataMatrix}
\title{dHOMDataMatrix}
\value{
reference class of type openmi.om.base.
}
\description{
Simple class to hold tabular values
}
\details{
Matrix meta-model object

Has standard methods for managing data and meta data
}
\examples{
NA
}
\seealso{
NA
}
\section{Super class}{
\code{\link[hydrotools:dHVariablePluginDefault]{hydrotools::dHVariablePluginDefault}} -> \code{dHOMDataMatrix}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{what is it called}

\item{\code{object_class}}{model object type}

\item{\code{entity_bundle}}{model object type}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-dHOMDataMatrix-exportOpenMIBase}{\code{dHOMDataMatrix$exportOpenMIBase()}}
\item \href{#method-dHOMDataMatrix-param_info}{\code{dHOMDataMatrix$param_info()}}
\item \href{#method-dHOMDataMatrix-clone}{\code{dHOMDataMatrix$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="dHVariablePluginDefault" data-id="exportOpenMI"><a href='../../hydrotools/html/dHVariablePluginDefault.html#method-dHVariablePluginDefault-exportOpenMI'><code>hydrotools::dHVariablePluginDefault$exportOpenMI()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="dHVariablePluginDefault" data-id="exportOpenMIpost"><a href='../../hydrotools/html/dHVariablePluginDefault.html#method-dHVariablePluginDefault-exportOpenMIpost'><code>hydrotools::dHVariablePluginDefault$exportOpenMIpost()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="dHVariablePluginDefault" data-id="fromOpenMIBase"><a href='../../hydrotools/html/dHVariablePluginDefault.html#method-dHVariablePluginDefault-fromOpenMIBase'><code>hydrotools::dHVariablePluginDefault$fromOpenMIBase()</code></a></span></li>
<li><span class="pkg-link" data-pkg="hydrotools" data-topic="dHVariablePluginDefault" data-id="initialize"><a href='../../hydrotools/html/dHVariablePluginDefault.html#method-dHVariablePluginDefault-initialize'><code>hydrotools::dHVariablePluginDefault$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-dHOMDataMatrix-exportOpenMIBase"></a>}}
\if{latex}{\out{\hypertarget{method-dHOMDataMatrix-exportOpenMIBase}{}}}
\subsection{Method \code{exportOpenMIBase()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{dHOMDataMatrix$exportOpenMIBase(entity)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{entity}}{the local object to work on}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
an updated config if necessary or FALSE if it fails
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-dHOMDataMatrix-param_info"></a>}}
\if{latex}{\out{\hypertarget{method-dHOMDataMatrix-param_info}{}}}
\subsection{Method \code{param_info()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{dHOMDataMatrix$param_info()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
info regarding the needs and capabilities of this object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-dHOMDataMatrix-clone"></a>}}
\if{latex}{\out{\hypertarget{method-dHOMDataMatrix-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{dHOMDataMatrix$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
