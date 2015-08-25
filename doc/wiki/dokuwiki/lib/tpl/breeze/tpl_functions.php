<?php
/**
 * Template Functions
 *
 * This file provides template specific custom functions that are
 * not provided by the DokuWiki core.
 * It is common practice to start each function with an underscore
 * to make sure it won't interfere with future core functions.
 */

// must be run from within DokuWiki
if (!defined('DOKU_INC')) die();

/**
 * Create link/button to discussion page and back
 *
 * @author Anika Henke <anika@selfthinker.org>
 */
function _tpl_discussion($discussionPage, $title, $backTitle, $link=0, $wrapper=0) {
    global $ID;

    $discussPage    = str_replace('@ID@', $ID, $discussionPage);
    $discussPageRaw = str_replace('@ID@', '', $discussionPage);
    $isDiscussPage  = strpos($ID, $discussPageRaw) !== false;
    $backID         = ':'.str_replace($discussPageRaw, '', $ID);

    if ($wrapper) echo "<$wrapper>";

    if ($isDiscussPage) {
        if ($link)
            tpl_pagelink($backID, $backTitle);
        else
            echo html_btn('back2article', $backID, '', array(), 'get', 0, $backTitle);
    } else {
        if ($link)
            tpl_pagelink($discussPage, $title);
        else
            echo html_btn('discussion', $discussPage, '', array(), 'get', 0, $title);
    }

    if ($wrapper) echo "</$wrapper>";
}

/**
 * Create link/button to user page
 *
 * @author Anika Henke <anika@selfthinker.org>
 */
function _tpl_userpage($userPage, $title, $link=0, $wrapper=0) {
    if (empty($_SERVER['REMOTE_USER'])) return;

    global $conf;
    $userPage = str_replace('@USER@', $_SERVER['REMOTE_USER'], $userPage);

    if ($wrapper) echo "<$wrapper>";

    if ($link)
        tpl_pagelink($userPage, $title);
    else
        echo html_btn('userpage', $userPage, '', array(), 'get', 0, $title);

    if ($wrapper) echo "</$wrapper>";
}

/**
 * Wrapper around custom template actions
 *
 * @author Anika Henke <anika@selfthinker.org>
 */
function _tpl_action($type, $link=0, $wrapper=0) {
    switch ($type) {
        case 'discussion':
            if (tpl_getConf('discussionPage')) {
                _tpl_discussion(tpl_getConf('discussionPage'), tpl_getLang('discussion'), tpl_getLang('back_to_article'), $link, $wrapper);
            }
            break;
        case 'userpage':
            if (tpl_getConf('userPage')) {
                _tpl_userpage(tpl_getConf('userPage'), tpl_getLang('userpage'), $link, $wrapper);
            }
            break;
    }
}

/**
 * Create event for tools menues
 *
 * @author Anika Henke <anika@selfthinker.org>
 */
function _tpl_toolsevent($toolsname, $items, $view='main') {
    $data = array(
        'view'  => $view,
        'items' => $items
    );

    $hook = 'TEMPLATE_'.strtoupper($toolsname).'_DISPLAY';
    $evt = new Doku_Event($hook, $data);
    if($evt->advise_before()){
        foreach($evt->data['items'] as $k => $html) echo $html;
    }
    $evt->advise_after();
}

/**
 * copied from core (available since Binky)
 */
if (!function_exists('tpl_classes')) {
    function tpl_classes() {
        global $ACT, $conf, $ID, $INFO;
        $classes = array(
            'dokuwiki',
            'mode_'.$ACT,
            'tpl_'.$conf['template'],
            !empty($_SERVER['REMOTE_USER']) ? 'loggedIn' : '',
            $INFO['exists'] ? '' : 'notFound',
            ($ID == $conf['start']) ? 'home' : '',
        );
        return join(' ', $classes);
    }
}

/**
 * Special Functions for this template
 *
 * @author Florian Wehner <florian@whnr.de>
 *
 */

// If output exists, put the content of $before and $after around it
function _tpl_check_if_output_and_amend($before="", $tpl_function="", $after="") {
    ob_start();
    eval($tpl_function.";");
    $function_output = ob_get_clean(); 
    
    if(strlen($function_output)==0) return false;

    print $before;
    print $function_output;
    print $after;
    return true;
}

// Copied and adjusted from core for navbar search form
function _tpl_draw_searchform($ajax = true, $autocomplete = true) {
    global $lang;
    global $ACT;
    global $QUERY;

    // don't print the search form if search action has been disabled
    if(!actionOK('search')) return false;

    print '<form action="'.wl().'" accept-charset="utf-8" class="search" id="dw__search" method="get" role="search"><div class="field">';
    print '<input type="hidden" name="do" value="search" />';
    print '<input type="search" ';
    if($ACT == 'search') print 'value="'.htmlspecialchars($QUERY).'" ';
    if(!$autocomplete) print 'autocomplete="off" ';
    print 'id="qsearch__in" accesskey="f" name="id" class="search input" title="[F]" placeholder="'.$lang['btn_search'].'"/>';
    if($ajax) print '<div id="qsearch__out" class="ajax_qsearch JSpopup"></div>';
    print '</div></form>';
    return true;
}
