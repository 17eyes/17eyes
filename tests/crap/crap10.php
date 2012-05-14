<?php

switch (true) {
        case $formid == 'search_form' :
        case $formid == 'search_theme_form' :
            $form['#action'] = getlangpref() . ltrim($form['#action'], '/');
            $form['#submit']['gpcustom_customsubmit'] = array();
            break;
        case $formid == 'localizernode_translations' :
            foreach ( $form['languages'] as $key => $value ) {
                if ( !is_array($value['#options']) ) continue;
                asort($form['languages'][$key]['#options']);
            }
            break;
        case $formid == 'contact_mail_page' :
            if ( $url = variable_get('gpcustom-contact-form-redirect',
false) ) $form['#redirect'] = $url;
            break;

    }
