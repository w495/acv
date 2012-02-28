/* ************************************************************************
************************************************************************ */

qx.Class.define("bsk.view.SelListTreeDirs",
{
    extend : qx.ui.tree.Tree,
 
    /**
     *
     *  @param root --- виджет из которого вызвали.
     *  @param url --- url запроса.
     *  @param labelFieldName
     *  @param descrFieldName
     *  @param paramdict --- список параметоров url (может быть undefined)
     */
    construct : function(root, url, labelFieldName, descrFieldName, paramdict) {
        this.biz = root;
        this.url = url;
        this.labelFieldName = labelFieldName;
        this.descrFieldName = descrFieldName;
        this.paramdict = paramdict;
        
        this.base(arguments);
        this.setHideRoot(true);
        this.setOpenMode("click");
        this.addListener("changeSelection", this._onMenuSelect, this);

        this.root = new qx.ui.tree.TreeFolder();
        this.setRoot(this.root);
        this.root.setOpen(true);
        this.data = {};
        
        console.log("this.url", this.url);
        
        if(this.url)
            this._requestItems(this.url);
        
    },

    members : {
        
        _onMenuSelect : function(e) {
            
        },
        
        reset : function() {
            this.setHideRoot(true);
            this.setOpenMode("click");

            this.root = new qx.ui.tree.TreeFolder();
            this.setRoot(this.root);
            this.root.setOpen(true);
            this.data = {};
            if(this.url)
                this._requestItems(this.url);
        },
        
        requestItems : function() {
            this._requestItems(this.url);
        },
        
        _requestItems : function(url) {
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            for(var key in this.paramdict){
                req.setParameter(key, this.paramdict[key]);
            }
            req.addListener("completed", this._onIncomeItems, this);
            req.send();
        },
        
        _requestItemsDir : function(url, id) {
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            for(var key in this.paramdict){
                req.setParameter(key, this.paramdict[key]);
            }
            req.setParameter("id", id);
            req.addListener("completed", this._onIncomeItemsGen(id), this);
            req.send();
        },
        
        setParamdict: function(paramdict) {
            this.paramdict = paramdict;
        },
        
        _onIncomeItems : function(response) {
            var result = response.getContent();
            if (false == bsk.util.errors.process(this, result) )
                return false;
            this.data = {};
            this.addItems(result.values);
            if(this.biz["on_selDataLoaded"])
                this.biz.on_selDataLoaded(this);
            return true;
        },

        _onIncomeItemsGen : function(id) {
            var this_ = this;
            return function(response) {
                var result = response.getContent();
                console.log(result.values);
                //if (false == bsk.util.errors.process(this_, result) )
                //    return false;
                this_.data = {};
                this_.addItems(this_.data[id].item, result.values);
                if(this_.biz["on_selDataLoaded"])
                    this_.biz.on_selDataLoaded(this_);
                return true;
            }
        },
        
        addItems : function(values) {
            return this.addItemsDir(this.root, values);
        },
        
        addItemsDir : function(a_root, values) {
            for(var i=0; i<values.length; i++) {
                var E = values[i];
                if(this.data[E.id] != undefined)
                    continue;
                
                var item = this.mkItem(E)
                var this_ = this;
                
                item.addListenerOnce("click", function(){
                    alert("" + E.id);
                    this_._requestItemsDir(this_.url, E.id);
                    this.add(new qx.ui.tree.TreeFolder("sdsd"));
                }, item);
                
                a_root.add(item);
            }
        },
        
        mkItem : function(E) {
            var item = new qx.ui.tree.TreeFolder();
            var checkbox = new qx.ui.form.CheckBox();
            checkbox.setFocusable(false);
            checkbox.bsk_element = E;
            checkbox.item = item;
            item.setIcon(null);
            item.addWidget(checkbox);
            item.addLabel("" + E[this.labelFieldName]);
            item.addWidget(new qx.ui.core.Spacer(), {flex: 1});
            var text = new qx.ui.basic.Label(E[this.descrFieldName]);//alias);
            text.setWidth(150);
            item.addWidget(text);
            this.data[E.id] = checkbox;
            
            return item;
        },
        
        remItem : function(value) {
            var newData = {};
            var E = value;
            if(this.data[value.id] == undefined)
                return false;
            var checkbox = this.data[value.id];
            this.root.remove(checkbox.item);
            this.data[value.id] = undefined;
            for(var key in this.data) {
                var E = this.data[key];
                if(E != undefined) 
                    newData[key] = E;
            }
            this.data = newData;
            return true;
        },
        
        remItems : function(values) {
            var newData = {};
            for(var i=0; i<values.length; i++) {
                var E = values[i];
                if(this.data[E.id] == undefined)
                    continue;
                var checkbox = this.data[E.id];
                this.root.remove(checkbox.item);
                this.data[E.id] = undefined;
            }

            for(var key in this.data) {
                var E = this.data[key];
                if(E != undefined) 
                    newData[key] = E;
            }
            this.data = newData;
        },

        getSelectedId : function() {
            var ret = [];
            for(var key in this.data) {
                var cb = this.data[key];
                if(cb.getValue() == true) {
                    ret.push(cb.bsk_element.id);
                }
            }
            return ret;
        },

        getAllId : function() {
            var ret = [];
            for(var key in this.data) {
                var cb = this.data[key];
                ret.push(cb.bsk_element.id);
            }
            return ret;
        },

        getSelected : function() {
            var ret = [];
            for(var key in this.data) {
                var cb = this.data[key];
                if(cb.getValue() == true) {
                    ret.push(cb.bsk_element);
                }
            }
            return ret;
        },

        setChecked : function(idList) {
            for(var i in this.data)
                this.data[i].setValue(false);
                
            for(var i=0; i < idList.length; i++) {
                var id=idList[i];
                if(this.data[id] != undefined)
                    this.data[id].setValue(true);
            }
        },
        
        configureTreeItem : function(treeItem, vLabel, vIcon)
        {
          // A left-justified icon
          if (Math.floor(Math.random() * 4) == 0) {
            var img = new qx.ui.basic.Image("icon/16/status/dialog-information.png");
            treeItem.addWidget(img);
          } else {
            treeItem.addWidget(new qx.ui.core.Spacer(16, 16));
          }
    
          // Here's our indentation and tree-lines
          treeItem.addSpacer();
    
          if (treeItem instanceof qx.ui.tree.TreeFolder) {
            treeItem.addOpenButton();
          }
    
          // The standard tree icon follows
          treeItem.addIcon();
          treeItem.setIcon(arguments.length >= 3 ? vIcon : "icon/16/places/user-desktop.png");
          
          // A checkbox comes right after the tree icon
          var checkbox = new qx.ui.form.CheckBox();
          checkbox.setFocusable(false);
          treeItem.addWidget(checkbox);
    
          // The label
          treeItem.addLabel(vLabel);
    
          // All else should be right justified
          treeItem.addWidget(new qx.ui.core.Spacer(), {flex: 1});
    
          // Add a file size, date and mode
          var text = new qx.ui.basic.Label(Math.round(Math.random() * 100) + "kb");
          text.setWidth(50);
          treeItem.addWidget(text);
    
          text = new qx.ui.basic.Label("May " + Math.round(Math.random() * 30 + 1) + " 2005");
          text.setWidth(150);
          treeItem.addWidget(text);
    
          text = new qx.ui.basic.Label("-rw-r--r--");
          text.setWidth(80);
          treeItem.addWidget(text);
    
          return treeItem;
        }

    }
});


