(in-package "CLOG-TOOLS")
(defclass quick-start (clog:clog-panel)
  (    (Instructions :reader Instructions)
    (span-13 :reader span-13)
    (image-12 :reader image-12)
))
(defun create-quick-start (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel (change-class (clog:create-div clog-obj :content "<img src=\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABwAAAAcCAMAAABF0y+mAAABf2lDQ1BzUkdCIElFQzYxOTY2LTIuMQAAKJF1kc8rRFEUxz8zgxEjioVkMQkbM2LUxEYZCSVNY5TBZuaZH2p+vN4bSbbKdooSG78W/AVslbVSREqWsiY26DnXqJHMvd17Pvd7zzmdey7Yw2ktY1b0QCabN0KjAfdMZNbtfKSKFhxAV1Qz9aFgcIKy4+0Gm7JXXpWrvN+/o3YhbmpgqxYe1HQjLzwmPLGc1xVvCjdpqeiC8LGwx5ACha+VHivyk+JkkT8UG+HQMNgbhN3JXxz7xVrKyAjLy2nPpJe0n3rUS1zx7PSU2DZZrZiEGCWAm3FGGMZPLwOy+/Hio1tOlInv+Y6fJCexmuw6KxgskiRFHo+oS5I9LjYhelxmmhXV/799NRN9vmJ2VwAqHyzrpQOcG/BZsKz3fcv6PADHPZxlS/G5Peh/Fb1Q0tp3oX4NTs5LWmwLTteh+U6PGtFvSf28PZGA5yOoi0DjJdTMFXv2c8/hLYRX5asuYHsHOsW/fv4LI2xnxwFcS14AAADkUExURQcFAEEwASwgAD4uAU05AVZAAiEYAKuABJJuBOeuB/2+CAwIADYoAayBBP6/CGBIA1A8AfS4CFA7AR4WAGtPAigeATUnAfm7COCpB6p/BOWsB3VYBA8LAD0uAtWgByQbARkTAcmXBpJtBAgFAN6nB6B4BZBsBYFhBHJWBGNLA1U/Av/ACAQDAF9IA8KSBu+0B553BRsUAVE9A86bBrKGBgkHAKZ9BZ12Bfe6CNahBxMOAQMCAHdaBOatB5t1BQAAANymBwsIAMWUBsqYBr+QBl5HAzcpAvK2CNKeB8+cBumvByZFyZjjEKMAAABMdFJOU////////////////////////////////////////////////////////////////////////////////////////////////////wCejeTMAAAACXBIWXMAAC4jAAAuIwF4pT92AAAA40lEQVQokYWSyQ6CMBCG56ShhxoORoJ7oqJWFEXcoe4aef/3scViZavfhdIv07T/DIQKQC41EqHlSEy+4JTUSQL9VyKSAkmZccJC9kx5Mpc/WzXju/xIeU+z3qBNU94ZZGHLBsrodHFcCqHW6w+s4WhcpQJw4jQgbE1oGns6i0ohdDKOM1+4XC5zJaXejMlugaQrJo0iuWZyUyS3/Ck78bNPuoPPQwhK/DHtsn60vNicKmci4kP+5bphgWHzFpXfgwcW8cngMUKPp+u+2BeTbMui7UTLlM1Wj4l6wNSj+Weo83gDWkirJ693IE0AAAAASUVORK5CYII=\" alt=\"Add image url\" style=\"box-sizing: content-box; position: static; left: 93px; top: 29px;\" id=\"CLOGB3865888353\" data-clog-name=\"image-12\"><span style=\"box-sizing: content-box; position: static; left: 38px; top: 13px;\" id=\"CLOGB3865888354\" data-clog-name=\"span-13\">&nbsp;<b id=\"CLOGB3865888355\" data-clog-name=\"none-b-1\">CLOG The Common Lisp Omnificent GUI</b></span><div id=\"CLOGB3865888356\" data-clog-name=\"Instructions\">
<h2 id=\"CLOGB3865888357\" data-clog-name=\"none-h2-1-11\">Quick Start</h2>
<h3 style=\"left: -75px; top: 298.5px;\" id=\"CLOGB3865888358\" data-clog-name=\"none-h3-2-11\">Placing Controls</h3>
<b id=\"CLOGB3865888359\" data-clog-name=\"none-b-3-11\">Placing Controls</b> - Click a control from the control pallets in the top left panel. Click on your panel<br id=\"CLOGB3865888360\" data-clog-name=\"none-br-4-11\">
<b id=\"CLOGB3865888361\" data-clog-name=\"none-b-5-11\">Static Placement</b> - Click a control from the control pallets in the top left panel. Click on your panel while holding down &lt;ctrl/cmd&gt;<br id=\"CLOGB3865888362\" data-clog-name=\"none-br-6-11\">
<b id=\"CLOGB3865888363\" data-clog-name=\"none-b-7-11\">Place as child control</b> - Click the parent control in your panel. Click a control from the control pallets in the top left panel. Click on your panel while holding down &lt;shift&gt;<br id=\"CLOGB3865888364\" data-clog-name=\"none-br-8-11\">
Place as static and child control - Click the parent control in your panel. Click a control from the control pallets in the top left panel. Click on your panel while holding down &lt;shift&gt;+&lt;ctrl/cmd&gt;<br id=\"CLOGB3865888365\" data-clog-name=\"none-br-9-11\">
<br id=\"CLOGB3865888366\" data-clog-name=\"none-br-10-11\">
<h3 id=\"CLOGB3865888367\" data-clog-name=\"none-h3-11-11\">Existing Controls as Children</h3>
<b id=\"CLOGB3865888368\" data-clog-name=\"none-b-12-11\">2 ways:</b><br id=\"CLOGB3865888369\" data-clog-name=\"none-br-13-11\">
1. Click the parent control in your panel. Holding &lt;shift&gt; click the control to make a child.<br id=\"CLOGB3865888370\" data-clog-name=\"none-br-14-11\">
2. Use the control list in the lower left to drag and drop holding the &lt;shift&gt; key<br id=\"CLOGB3865888371\" data-clog-name=\"none-br-15-11\">
<br id=\"CLOGB3865888372\" data-clog-name=\"none-br-16-11\">
<h3 id=\"CLOGB3865888373\" data-clog-name=\"none-h3-17-11\">Existing Controls as Static</h3>
Use the positioning property in the property panel<br id=\"CLOGB3865888374\" data-clog-name=\"none-br-18-11\">
<br id=\"CLOGB3865888375\" data-clog-name=\"none-br-19-11\">
<h3 id=\"CLOGB3865888376\" data-clog-name=\"none-h3-20-11\">Access Children Controls</h3>
Double click the parent control, it will freeze (unfreeze by double clicking it in the control list in the lower left panel), and children can not be manipulated<br id=\"CLOGB3865888377\" data-clog-name=\"none-br-21-11\">
<br id=\"CLOGB3865888378\" data-clog-name=\"none-br-22-11\">
<h3 id=\"CLOGB3865888379\" data-clog-name=\"none-h3-23-11\">Change Creation/Tab Order</h3>
Use the control list in lower left panel to drag and drop order<br id=\"CLOGB3865888380\" data-clog-name=\"none-br-24-11\">
</div>"
         :hidden hidden :class class :html-id html-id :auto-place auto-place) 'quick-start)))
    (setf (slot-value panel 'Instructions) (attach-as-child clog-obj "CLOGB3865888356" :clog-type 'CLOG:CLOG-DIV :new-id t))
    (setf (slot-value panel 'span-13) (attach-as-child clog-obj "CLOGB3865888354" :clog-type 'CLOG:CLOG-SPAN :new-id t))
    (setf (slot-value panel 'image-12) (attach-as-child clog-obj "CLOGB3865888353" :clog-type 'CLOG:CLOG-IMG :new-id t))
    panel))
