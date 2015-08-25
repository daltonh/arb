<?php
/**
 * DokuWiki Clean Template
 *
 * @link     FIXME 
 * @author   FIXME 
 * @license  FIXME 
 */

// error_reporting(E_ALL & ~E_DEPRECATED & ~E_NOTICE); ini_set('display_errors', '1');  // Switch on for error reporting

if (!defined('DOKU_INC')) die(); /* must be run from within DokuWiki */
@require_once(dirname(__FILE__).'/tpl_functions.php'); /* include hook for template functions */
?>
<!DOCTYPE html>
<!-- paulirish.com/2008/conditional-stylesheets-vs-css-hacks-answer-neither/ -->
<!--[if lt IE 7]> <html class="no-js ie6 oldie" lang="en"> <![endif]-->
<!--[if IE 7]>    <html class="no-js ie7 oldie" lang="en"> <![endif]-->
<!--[if IE 8]>    <html class="no-js ie8 oldie" lang="en"> <![endif]-->
<!--[if IE 9]>    <html class="no-js ie9" lang="en"> <![endif]-->
<!-- Consider adding an manifest.appcache: h5bp.com/d/Offline -->
<!--[if gt IE 9]><!--> <html class="no-js" lang="en" itemscope="" itemtype="http://schema.org/Product"> <!--<![endif]-->
<html
    xmlns="http://www.w3.org/1999/xhtml"
    xml:lang="<?php echo $conf['lang']?>"
    lang="<?php echo $conf['lang']?>"
    dir="<?php echo $lang['direction']?>">
    <head>
        <title><?php tpl_pagetitle()?> - <?php echo hsc($conf['title'])?></title>
        <meta charset="utf-8">

        <!-- Use the .htaccess and remove these lines to avoid edge case issues.
                         More info: h5bp.com/b/378 -->
        <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">

        <meta name="description" content="">
        <meta name="keywords" content="">
        <meta name="author" content="humans.txt">

        <link rel="shortcut icon" href="favicon.png" type="image/x-icon">

        <!-- Facebook Metadata /-->
        <meta property="fb:page_id" content="">
        <meta property="og:image" content="">
        <meta property="og:description" content="">
        <meta property="og:title" content="">

        <!-- Google+ Metadata /-->
        <meta itemprop="name" content="">
        <meta itemprop="description" content="">
        <meta itemprop="image" content="">

        <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1">

        <?php tpl_metaheaders()?>

        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/modernizr-2.6.2.min.js"></script>
    </head>
    <body>
        <div class="dokuwiki">
            <div class="navcontain">
                <nav class="navbar" gumby-fixed="top" id="wiki__nav">
                    <a class="toggle" gumby-trigger="#wiki__nav > .row > ul" href="#"><i class="icon-menu"></i></a>
                    <div class="row">
                        <h3 class="five columns logo" id="nav__logo">
                            <a href=<?php echo wl()?>><?php echo hsc($conf['title'])?></a>
                        </h3>
                        <ul class="seven columns" id="nav__menu">
                            <li><a href="#">Wikitools</a>
                                <div class="dropdown">
                                    <ul>
                                        <li><?php tpl_actionlink('recent')?></li>
                                        <li><?php tpl_actionlink('profile')?></li>
                                        <li><?php tpl_actionlink('login')?></li>
                                        <li><?php tpl_actionlink('admin')?></li>
                                        <li><?php tpl_actionlink('index')?></li>
                                    </ul>
                                </div>
                            </li>
                            <li><a href="#">Pagetools</a>
                                <div class="dropdown">
                                    <ul>
                                        <li><?php tpl_actionlink('edit')?></li>
                                        <li><?php tpl_actionlink('history')?></li>
                                        <li><?php tpl_link(wl($ID,'do=backlink'),"Backlinks")?></li>
                                        <li><?php tpl_actionlink('subscribe')?></li>
                                    </ul>
                                </div>
                            </li>
                            <li class="field">
                                <?php _tpl_draw_searchform()?>
                            </li>
                        </ul>
                    </div>
                </nav>
            </div>
            <div class="row">
                <?php html_msgarea()?>
            </div>
            <div class="row" id="status__bar">
                <?php if ($conf['breadcrumbs']) { tpl_breadcrumbs(); } ?>
                <?php if ($conf['youarehere']) { tpl_youarehere(); } ?>
            </div>
            <div class="row">
<?php
// render the content into buffer for later use
ob_start();
tpl_content(false);
$buffer = ob_get_clean();
?>

                <div class="three columns" id="dokuwiki__sidebar">
                        <div class="row" id="toc__container">
                            <hr>
                            <h4 class="toggle" gumby-trigger="#dw__toc" style="cursor:pointer" id="toc__header"><?php echo $lang['toc']?>
                                <i class="icon-right-open"></i>
                            </h4>
                            <?php tpl_toc()?>
                            <hr>
                        </div>
                        <div class="row hide-on-phones" gumby-fixed="top" gumby-top="60" gumby-offset="60" style="text-align: center;">
                                <a href="#" class="skip" gumby-goto="top" gumby-duration="600"><i class="icon icon-up-open-mini"></i><?php echo $lang['btn_top']?><i class="icon icon-up-open-mini"></i></a>
                        </div>
                </div> 
                <div class="nine columns" id="dokuwiki__content">
                    <?php echo $buffer?>
                    <div class="row" id="page__info">
                        <?php tpl_userinfo()?> – <?php tpl_pageinfo()?>
                    </div>
                </div> 
            </div>
            <?php tpl_indexerWebBug(); ?>
        </div>

        <!-- Grab Google CDN's jQuery, fall back to local if offline -->
        <!-- 2.0 for modern browsers, 1.10 for .oldie -->
<script>
var oldieCheck = Boolean(document.getElementsByTagName('html')[0].className.match(/\soldie\s/g));
if(!oldieCheck) {
    document.write('<script src="//ajax.googleapis.com/ajax/libs/jquery/2.0.2/jquery.min.js"><\/script>');
} else {
    document.write('<script src="//ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"><\/script>');
}
</script>
<script>
if(!window.jQuery) {
    if(!oldieCheck) {
        document.write('<script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/jquery-2.0.2.min.js"><\/script>');
    } else {
        document.write('<script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/jquery-1.10.1.min.js"><\/script>');
    }
}
</script>

        <!--
        Include gumby.js followed by UI modules followed by gumby.init.js
        Or concatenate and minify into a single file -->
        <script gumby-touch="js/libs" src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/gumby.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/gumby.retina.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/gumby.fixed.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/gumby.skiplink.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/gumby.toggleswitch.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/gumby.checkbox.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/gumby.radiobtn.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/gumby.tabs.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/gumby.navbar.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/ui/jquery.validation.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/libs/gumby.init.js"></script>
        <script src="<?php echo tpl_basedir()?>js/gumby.min.js"></script>

        <!--
        Google's recommended deferred loading of JS
        gumby.min.js contains gumby.js, all UI modules and gumby.init.js

        Note: If you opt to use this method of defered loading,
        ensure that any javascript essential to the initial
        display of the page is included separately in a normal
        script tag.

<script type="text/javascript">
function downloadJSAtOnload() {
    var element = document.createElement("script");
    element.src = "js/libs/gumby.min.js";
    document.body.appendChild(element);
        }
        if (window.addEventListener)
            window.addEventListener("load", downloadJSAtOnload, false);
        else if (window.attachEvent)
            window.attachEvent("onload", downloadJSAtOnload);
        else window.onload = downloadJSAtOnload;
        </script> -->

        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/plugins.js"></script>
        <script src="<?php echo tpl_basedir()?>bower_components/gumby/js/main.js"></script>

        <!-- Change UA-XXXXX-X to be your site's ID -->
        <!--<script>
        window._gaq = [['_setAccount','UAXXXXXXXX1'],['_trackPageview'],['_trackPageLoadTime']];
Modernizr.load({
    load: ('https:' == location.protocol ? '//ssl' : '//www') + '.google-analytics.com/ga.js'
        });
        </script>-->

        <!-- Prompt IE 6 users to install Chrome Frame. Remove this if you want to support IE 6.
           chromium.org/developers/how-tos/chrome-frame-getting-started -->
        <!--[if lt IE 7 ]>
        <script src="//ajax.googleapis.com/ajax/libs/chrome-frame/1.0.3/CFInstall.min.js"></script>
        <script>window.attachEvent('onload',function(){CFInstall.check({mode:'overlay'})})</script>
        <![endif]-->
    </body>
</html>
